#lang racket/base
;; Lowest Common Denominator terminal.

(module+ event-structs
  (provide (struct-out key)
           (struct-out any-mouse-event)
           (struct-out mouse-focus-event)
           (struct-out mouse-event)
           (struct-out unknown-escape-sequence)
           (struct-out position-report)
           (struct-out screen-size-report)
           (struct-out kitty-key-event)
           (struct-out bracketed-paste-event)))
(provide (struct-out key)
         (struct-out any-mouse-event)
         (struct-out mouse-focus-event)
         (struct-out mouse-event)
         (struct-out unknown-escape-sequence)
         (struct-out position-report)
         (struct-out screen-size-report)
         (struct-out kitty-key-event)
         (struct-out bracketed-paste-event)
         add-modifier
         lex-lcd-input
         lcd-terminal-utf-8?
         lcd-terminal-basic-x11-mouse-support?
         lcd-terminal-escape-delay-ms)

(require racket/set)
(require racket/match)
(require (only-in racket/string string-split))
(require (only-in racket/port input-port-append peek-bytes-evt))

(define lcd-terminal-utf-8? (make-parameter #t))

;; Delay in milliseconds to wait after receiving a bare ESC (0x1b)
;; before deciding it is a standalone Escape key press rather than the
;; start of a multi-byte escape sequence.  Over SSH, escape sequences
;; can be "torn" across multiple reads, so a non-zero delay is needed
;; to heuristically distinguish the two cases.  Set to 0 to disable
;; the delay (treats a lone ESC as standalone immediately when no more
;; bytes are buffered).
(define lcd-terminal-escape-delay-ms (make-parameter 50))

(define lcd-terminal-basic-x11-mouse-support?
  (make-parameter
   (match (getenv "TERM")
     [(pregexp #px"^st-.*") #f]
     ;; ^ basic mouse events OVERLAP with control-delete in st!
     ;; This isn't a problem for SGR mouse event reports, though.
     [_ #t])))

(struct unknown-escape-sequence (string) #:prefab)
(struct key (value modifiers) #:prefab)
(struct position-report (row column) #:prefab)
(struct screen-size-report (rows columns) #:prefab)

(struct any-mouse-event () #:prefab)
(struct mouse-focus-event any-mouse-event (focus-in?) #:prefab)
(struct mouse-event any-mouse-event (type button row column modifiers) #:prefab)

;; Kitty keyboard protocol event (CSI u sequences).
;; event-type is 'press, 'repeat, or 'release.
;; modifiers is a set of symbols like key's modifiers.
(struct kitty-key-event (value modifiers event-type) #:prefab)

;; Bracketed paste event (CSI 200~ ... CSI 201~).
(struct bracketed-paste-event (content) #:prefab)

(define (simple-key value) (key value (set)))
(define (S- value) (key value (set 'shift)))
(define (C- value) (key value (set 'control)))
(define (M- value) (key value (set 'meta)))
(define (C-S- value) (key value (set 'control 'shift)))
(define (C-M- value) (key value (set 'control 'meta)))

(define (add-modifier modifier k)
  (struct-copy key k [modifiers (set-add (key-modifiers k) modifier)]))

(define (decode-shifting-number v-plus-one k)
  (define v (- v-plus-one 1))
  (let* ((k (if (zero? (bitwise-and v 1)) k (add-modifier 'shift k)))
         (k (if (zero? (bitwise-and v 2)) k (add-modifier 'meta k)))
         (k (if (zero? (bitwise-and v 4)) k (add-modifier 'control k))))
    k))

(define (decode-shifting params value)
  (match params
    [(list 1 v-plus-one) (decode-shifting-number v-plus-one (simple-key value))]
    [_ (simple-key value)] ;; bit of a cop-out
    ))

(define (analyze-vt-tildeish-key lexeme params ctor)
  (match params
    [(list a b) (analyze-vt-tildeish-key* lexeme ctor a b)]
    [(list a) (analyze-vt-tildeish-key* lexeme ctor a 1)]
    [_ (simple-key (unknown-escape-sequence lexeme))]))

(define (analyze-vt-tildeish-key* lexeme ctor a b)
  (decode-shifting-number
   b
   (match a
     [1 (ctor 'home)] ;; linux console
     [2 (ctor 'insert)]
     [3 (ctor 'delete)]
     [4 (ctor 'end)] ;; linux console
     [5 (ctor 'page-up)]
     [6 (ctor 'page-down)]
     [7 (ctor 'home)]
     [8 (ctor 'end)]
     [11 (ctor 'f1)] [12 (ctor 'f2)] [13 (ctor 'f3)] [14 (ctor 'f4)]
     [15 (ctor 'f5)] [17 (ctor 'f6)] [18 (ctor 'f7)] [19 (ctor 'f8)]
     [20 (ctor 'f9)] [21 (ctor 'f10)] [23 (ctor 'f11)] [24 (ctor 'f12)]
     [25 (ctor 'f13)] [26 (ctor 'f14)] [28 (ctor 'f15)] [29 (ctor 'f16)]
     [31 (ctor 'f17)] [32 (ctor 'f18)] [33 (ctor 'f19)] [34 (ctor 'f20)]
     [_ (simple-key (unknown-escape-sequence lexeme))])))

(define (analyze-vt-bracket-key lexeme params mainchar)
  (match mainchar
    ["~" (analyze-vt-tildeish-key lexeme params simple-key)]
    ["$" (analyze-vt-tildeish-key lexeme params S-)]
    ["^" (analyze-vt-tildeish-key lexeme params C-)]
    ["@" (analyze-vt-tildeish-key lexeme params C-S-)]
    ["A" (decode-shifting params 'up)]
    ["B" (decode-shifting params 'down)]
    ["C" (decode-shifting params 'right)]
    ["D" (decode-shifting params 'left)]
    ["E" (decode-shifting params 'begin)]
    ["F" (decode-shifting params 'end)]
    ["G" (decode-shifting params 'begin)] ;; linux console (!)
    ["H" (decode-shifting params 'home)]
    ["I" (mouse-focus-event #t)]
    ["J" #:when (equal? params '(2)) (S- 'home)] ;; st, http://st.suckless.org/
    ["J" #:when (not params) (C- 'end)] ;; st, http://st.suckless.org/
    ["K" #:when (equal? params '(2)) (S- 'delete)] ;; st, http://st.suckless.org/
    ["K" #:when (not params) (S- 'end)] ;; st, http://st.suckless.org/
    ["L" (C- 'insert)]                  ;; st, http://st.suckless.org/
    ["M" (C- 'delete)]                  ;; st, http://st.suckless.org/. Overlaps with mouse events!
    ["O" (mouse-focus-event #f)]
    ["P" #:when (not params) (simple-key 'delete)] ;; st, http://st.suckless.org/
    ["P" (decode-shifting params 'f1)]
    ["Q" (decode-shifting params 'f2)]
    ["R" #:when (and (= (length params) 2) (> (car params) 1))
         (apply position-report params)]
    ["R" (decode-shifting params 'f3)]
    ["S" (decode-shifting params 'f4)]
    ["Z" (simple-key 'backtab)]
    ["a" (S- 'up)]
    ["b" (S- 'down)]
    ["c" (S- 'right)]
    ["d" (S- 'left)]
    ["h" #:when (equal? params '(4)) (simple-key 'insert)] ;; st, http://st.suckless.org/
    ["t" #:when (and (= (length params) 3) (= (car params) 8))
         (apply screen-size-report (cdr params))]
    [_ (simple-key (unknown-escape-sequence lexeme))]))

(define (analyze-vt-O-mainchar lexeme mainchar)
  (match mainchar
    ["a" (C- 'up)]
    ["b" (C- 'down)]
    ["c" (C- 'right)]
    ["d" (C- 'left)]

    ;; rxvt keypad keys.
    ;; Per http://www.vt100.net/docs/vt102-ug/appendixc.html, these
    ;; are "ANSI Alternate Keypad Mode" sequences.
    ["j" (simple-key #\*)]
    ["k" (simple-key #\+)]
    ["l" (simple-key #\,)] ;; my keypad doesn't have a comma
    ["m" (simple-key #\-)]
    ["n" (simple-key 'delete)] ;; #\.
    ["o" (simple-key #\/)]
    ["p" (simple-key 'insert)]       ;; #\0
    ["q" (simple-key 'end)]          ;; #\1
    ["r" (simple-key 'down)]         ;; #\2
    ["s" (simple-key 'page-down)]    ;; #\3
    ["t" (simple-key 'left)]         ;; #\4
    ["u" (simple-key 'begin)]        ;; #\5
    ["v" (simple-key 'right)]        ;; #\6
    ["w" (simple-key 'home)]         ;; #\7
    ["x" (simple-key 'up)]           ;; #\8
    ["y" (simple-key 'page-up)]      ;; #\9

    ["A" (simple-key 'up)]                            ;; kcuu1
    ["B" (simple-key 'down)]                          ;; kcud1
    ["C" (simple-key 'right)]                         ;; kcuf1
    ["D" (simple-key 'left)]                          ;; kcub1
    ["E" (simple-key 'begin)]                         ;; in screen
    ["F" (simple-key 'end)]                           ;; kend
    ["H" (simple-key 'home)]                          ;; khome
    ["M" (add-modifier 'control (simple-key #\M))] ;; keypad enter (rxvt)
    ["P" (simple-key 'f1)]
    ["Q" (simple-key 'f2)]
    ["R" (simple-key 'f3)]
    ["S" (simple-key 'f4)]
    [_ (simple-key (unknown-escape-sequence lexeme))]))

(define (interpret-ascii-code b)
  (cond
   [(= b #x09) (simple-key 'tab)]
   [(= b #x0a) (simple-key 'enter)]     ;; LF
   [(= b #x0d) (simple-key 'enter)]     ;; CR
   [(= b #x1b) (simple-key 'escape)]
   [(<= #x00 b #x1f) (C- (integer->char (+ b (char->integer #\A) -1)))]
   [(<= #x20 b #x7e) (simple-key (integer->char b))]
   [(= b #x7f) (simple-key 'backspace)]))

(define (decode-mouse-event-type type)
  (define type-code (arithmetic-shift type -5))
  (define modifier-code (bitwise-and (arithmetic-shift type -2) 7))
  (define modifiers
    (set-union (if (zero? (bitwise-and modifier-code 1)) (set) (set 'shift))
               (if (zero? (bitwise-and modifier-code 2)) (set) (set 'super))
               (if (zero? (bitwise-and modifier-code 4)) (set) (set 'control))))
  (define button (bitwise-and type 3))
  (match type-code
    [1 ;; Press or release
     (if (= button 3) ;; basic events don't distinguish specific release buttons
         (values 'release-all modifiers #f)
         (values 'press modifiers (+ button 1)))]
    [2 ;; Motion
     (values 'motion modifiers (if (= button 3) #f (+ button 1)))]
    [3 ;; Scroll (really, press events for buttons 4, 5)
     (values 'scroll modifiers (+ button 4))]
    [_
     (values #f modifiers (+ button 1))]))

(define (decode-basic-mouse-event lexeme event-bytes)
  (define-values (type modifiers button) (decode-mouse-event-type (bytes-ref event-bytes 0)))
  (define x-raw (bytes-ref event-bytes 1))
  (define y-raw (bytes-ref event-bytes 2))
  ;; Very large terminals (more than 256-32=224 columns/rows) report 0
  ;; for a column/row position when the mouse is to the right of the
  ;; maximum representable position. We report #f in these cases.
  (define x (if (zero? x-raw) #f (- x-raw 32)))
  (define y (if (zero? y-raw) #f (- y-raw 32)))
  (if (not type)
      (simple-key (unknown-escape-sequence lexeme))
      ;; x = column, y = row; struct fields are (type button row column modifiers)
      (mouse-event type button y x modifiers)))

(define (decode-extended-mouse-event lexeme type-byte x y release? input-next)
  (define-values (type modifiers button) (decode-mouse-event-type (+ type-byte 32)))
  (cond
    [(not type)
     (simple-key (unknown-escape-sequence lexeme))]
    [(eq? type 'release-all) ;; This is one of the things the extended format can do better!
     ;; x = column, y = row; struct fields are (type button row column modifiers)
     (mouse-event 'release button y x modifiers)]
    [(eq? type 'press)
     (mouse-event (if release? 'release 'press) button y x modifiers)]
    [release?
     ;; Ignore the event -- it's likely a spurious "scroll" release event from st
     (input-next)]
    [else
     (mouse-event type button y x modifiers)]))

;; Kitty keyboard protocol keycode mapping.
(define (kitty-keycode->value code)
  (cond
    [(and (>= code 32) (<= code 126)) (integer->char code)]
    [(= code 27) 'escape]
    [(= code 13) 'enter]
    [(= code 9) 'tab]
    [(= code 127) 'backspace]
    [(= code 57358) 'backtab]
    [(= code 57359) 'insert]
    [(= code 57360) 'delete]
    [(= code 57361) 'home]
    [(= code 57362) 'end]
    [(= code 57363) 'page-up]
    [(= code 57364) 'page-down]
    [(= code 57352) 'up]
    [(= code 57353) 'down]
    [(= code 57354) 'right]
    [(= code 57355) 'left]
    [(= code 57356) 'begin]
    [(and (> code 0) (<= code #x10FFFF)) (integer->char code)]
    [else 'unknown]))

;; Decode Kitty keyboard protocol parameters into a kitty-key-event.
(define (decode-kitty-key keycode modifiers-param event-type-num)
  (define mod-bits (sub1 (max 1 modifiers-param)))
  (define mods
    (set-union (if (zero? (bitwise-and mod-bits 1)) (set) (set 'shift))
               (if (zero? (bitwise-and mod-bits 2)) (set) (set 'meta))
               (if (zero? (bitwise-and mod-bits 4)) (set) (set 'control))))
  (define value (kitty-keycode->value keycode))
  (define event-type
    (case event-type-num
      [(2) 'repeat]
      [(3) 'release]
      [else 'press]))
  (kitty-key-event value mods event-type))

(define (lex-lcd-input port
                       #:utf-8? [utf-8? (lcd-terminal-utf-8?)]
                       #:basic-x11-mouse-support? [basic-x11-mouse-support?
                                                   (lcd-terminal-basic-x11-mouse-support?)])
  (lex-lcd-input* port port utf-8? basic-x11-mouse-support?))

;; Inner lexer.  `orig-port` is the actual input source; `port` is
;; what we read from — normally the same, but after waiting for a torn
;; escape sequence it may be a prepended port that re-supplies the ESC
;; byte we already consumed.
(define (lex-lcd-input* orig-port port utf-8? basic-x11-mouse-support?)
  (cond
   [(eof-object? (peek-byte port)) eof]
   ;; Standalone Escape detection: if the next byte is ESC, consume it
   ;; and check whether more bytes follow.  We must do this BEFORE
   ;; trying escape-sequence regexps because regexp-try-match blocks
   ;; on a file port when the pattern partially matches the leading ESC
   ;; byte but the rest of the sequence hasn't arrived yet.
   ;;
   ;; Only apply this when port eq? orig-port (the first call, not a
   ;; re-lex with a prepended port), to avoid infinite recursion.
   [(and (eq? port orig-port)
         (= (peek-byte port) #x1b))
    (read-byte port) ;; consume the ESC
    (cond
      ;; If more data is already buffered, re-lex with ESC prepended
      ;; so the escape-sequence regexps can match.
      [(and (byte-ready? orig-port) (not (eof-object? (peek-byte orig-port))))
       (lex-lcd-input* orig-port (input-port-append #f (open-input-bytes #"\e") orig-port)
                       utf-8? basic-x11-mouse-support?)]
      ;; No data yet — wait up to the escape delay for more bytes.
      [(let ([delay-ms (lcd-terminal-escape-delay-ms)])
         (and (> delay-ms 0)
              (let ([result (sync/timeout (/ delay-ms 1000.0)
                                         (peek-bytes-evt 1 0 #f orig-port))])
                (and result (not (eof-object? result))))))
       ;; More bytes arrived — re-lex with ESC prepended.
       (lex-lcd-input* orig-port (input-port-append #f (open-input-bytes #"\e") orig-port)
                       utf-8? basic-x11-mouse-support?)]
      ;; Timeout or zero delay — standalone Escape.
      [else
       (simple-key 'escape)])]
   [(regexp-try-match #px#"^\e\\[<([0-9]+);([0-9]+);([0-9]+)(m|M)" port) =>
    (lambda (match-result)
      (match-define (list lexeme type row column kind) match-result)
      (decode-extended-mouse-event lexeme
                                   (string->number (bytes->string/utf-8 type))
                                   (string->number (bytes->string/utf-8 row))
                                   (string->number (bytes->string/utf-8 column))
                                   (match kind [#"m" #t] [#"M" #f])
                                   (lambda ()
                                     (lex-lcd-input* orig-port port
                                                     utf-8?
                                                     basic-x11-mouse-support?))))]
   ;; Bracketed paste: ESC[200~ ... ESC[201~
   [(regexp-try-match #px#"^\e\\[200~" port) =>
    (lambda (_match-result)
      (define out (open-output-bytes))
      (let loop ()
        (cond
          [(regexp-try-match #px#"^\e\\[201~" port) (void)]
          [(eof-object? (peek-byte port)) (void)]
          [else (write-byte (read-byte port) out) (loop)]))
      (bracketed-paste-event (bytes->string/utf-8 (get-output-bytes out))))]
   ;; Kitty keyboard protocol query response: ESC[?Nu
   [(regexp-try-match #px#"^\e\\[\\?([0-9]+)u" port) =>
    (lambda (match-result)
      (match-define (list _lexeme flags-bytes) match-result)
      (define flags (string->number (bytes->string/utf-8 flags-bytes)))
      (kitty-key-event 'query-response (set) flags))]
   ;; Kitty keyboard protocol: ESC[keycode(;modifiers(:event_type)?)?u
   [(regexp-try-match #px#"^\e\\[([0-9]+)(;([0-9]+)(:([0-9]+))?)?u" port) =>
    (lambda (match-result)
      (match-define (list _lexeme keycode-bytes _ mod-bytes _ event-type-bytes) match-result)
      (define keycode (string->number (bytes->string/utf-8 keycode-bytes)))
      (define modifiers (if mod-bytes (string->number (bytes->string/utf-8 mod-bytes)) 1))
      (define event-type-num (and event-type-bytes
                                  (string->number (bytes->string/utf-8 event-type-bytes))))
      (decode-kitty-key keycode modifiers event-type-num))]
   [(and basic-x11-mouse-support?
         (regexp-try-match #px#"^\e\\[M(...)" port)) =>
    (lambda (match-result)
      (match-define (list lexeme mouse-event-bytes) match-result)
      (decode-basic-mouse-event lexeme mouse-event-bytes))]
   [(or (regexp-try-match #px"^\e\\[([0-9]+(;[0-9]+)*)?(.)" port)
        (regexp-try-match #px#"^\x9b([0-9]+(;[0-9]+)*)?(.)" port)) =>
    (lambda (match-result)
      (match-define (list lexeme parambytes _ mainbytes) match-result)
      (define params
        (and parambytes
             (map string->number (string-split (bytes->string/utf-8 parambytes) ";"))))
      (analyze-vt-bracket-key lexeme params (bytes->string/utf-8 mainbytes)))]
   [(regexp-try-match #px"^\eO([0-9])(.)" port) =>
    ;; screen generates shifting escapes for the keypad like this
    (lambda (match-result)
      (match-define (list lexeme v-plus-one-bytes mainbytes) match-result)
      (decode-shifting-number
       (string->number (bytes->string/utf-8 v-plus-one-bytes))
       (analyze-vt-O-mainchar lexeme (bytes->string/utf-8 mainbytes))))]
   [(regexp-try-match #px"^\eO(.)" port) =>
    (lambda (match-result)
      (match-define (list lexeme mainbytes) match-result)
      (analyze-vt-O-mainchar lexeme (bytes->string/utf-8 mainbytes)))]
   ;; Alt+key: ESC followed by a printable character or DEL.
   ;; This must come after CSI/SS3/mouse/kitty patterns so that
   ;; ESC [ and ESC O are not misinterpreted as Alt+[ and Alt+O.
   [(regexp-try-match #px#"^\e([\x20-\x7f])" port) =>
    (lambda (match-result)
      (match-define (list _lexeme char-bytes) match-result)
      (define b (bytes-ref char-bytes 0))
      (add-modifier 'meta (interpret-ascii-code b)))]
   ;; NOTE: Standalone ESC is now handled at the top of lex-lcd-input*
   ;; (before the regexp-try-match calls) to prevent blocking.
   ;; Characters between #\u80 and #\uff are ambiguous because in
   ;; some terminals, the high bit is set to indicate meta, and in
   ;; others, they are plain UTF-8 characters. We let the user
   ;; distinguish via the #:utf-8? keyword argument.
   [(not utf-8?)
    (define b (read-byte port))
    (if (< b 128)
        (interpret-ascii-code b)
        (add-modifier 'meta (interpret-ascii-code (- b 128))))]
   [else
    (define b (char->integer (read-char port)))
    (if (< b 128)
        (interpret-ascii-code b)
        (simple-key (integer->char b)))]))
