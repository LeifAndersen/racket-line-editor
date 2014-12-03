#lang racket/base

#;
(provide set-multi-line-mode
         line-editor)
(provide (all-defined-out))

(require racket/set
         racket/list
         racket/vector
         racket/file
         racket/port
         racket/match
         racket/format
         racket/string
         ffi/unsafe
         ffi/unsafe/define)

;; Based on liblinenoise: https://github.com/antirez/linenoise

(module+ test (require rackunit))

(define STDIN_FILENO 0)
(define VMIN 16)
(define VTIME 17)

(define TCSAFLUSH  #x2)

(define _tcflag_t _ulong)
(define BRKINT      #x2)       ; Input
(define INPCK       #x10)
(define ISTRIP      #x20)
(define ICRNL       #x100)
(define IXON        #x200)
(define OPOST       #x1)       ; Output
(define CS8         #x300)     ; Control
(define ECHO        #x8)       ; Local
(define ISIG        #x80)
(define ICANON      #x100)
(define IEXTEN      #x400)

(define _cc_t _ubyte)
(define _speed_t _ulong)
(define NCCS 20)

(define-cstruct _termios ([c_iflag _tcflag_t]
                          [c_oflag _tcflag_t]
                          [c_cflag _tcflag_t]
                          [c_lflag _tcflag_t]
                          [c_cc (_array/vector _cc_t NCCS)]
                          [c_ispeed _speed_t]
                          [c_ospeed _speed_t]))

(define-cstruct _winsize ([ws_row    _ushort]
                          [ws_col    _ushort]
                          [ws_xpixel _ushort]
                          [ws_ypixel _ushort]))

(define IOCPARM_MASK #x1fff)
(define IOC_OUT #x40000000)
(define (_IOC inout group num len)
  (bitwise-ior inout num (arithmetic-shift group 8)
               (arithmetic-shift (bitwise-and len IOCPARM_MASK) 16)))
(define (_IOR g n t)
  (_IOC IOC_OUT g n (ctype-sizeof t)))
(define TIOCGWINSZ (_IOR (char->integer #\t) 104 _winsize))

(define-ffi-definer define-libc (ffi-lib "libc"))
(define interfaces (make-hash))

(define-libc tcsetattr (_fun _int _int _termios-pointer -> _int))
(define-libc tcgetattr (_fun _int _termios-pointer -> _int))
(define-libc isatty (_fun _int -> _int))
(define (ioctl fildes request . args)
  (define itypes (list* _int _ulong
                        (map
                         (lambda (x)
                           (cond
                            [(winsize? x) _winsize-pointer]
                            [else (error 'ioctl
                                         "don't know how to deal with ~e" x)]))
                         args)))
  (define ioctl* (hash-ref interfaces itypes
                           (lambda ()
                             (let ([i (get-ffi-obj "ioctl" #f
                                                   (_cprocedure itypes _int))])
                               (hash-set! interfaces itypes i)
                               i))))
  (apply ioctl* fildes request args))

(define (read-response [port stdin])
  (let loop ([acc ""])
    (define x (read-char))
    (cond [(eof-object? x) x]
          [else (define acc* (string-append acc (~a x)))
                (if (equal? x #\R)
                    acc*
                    (loop acc*))])))

(define stdin (current-input-port))
(define stdout (current-output-port))
(define stderr (current-error-port))
(define unsupported-terminals (set "dumb" "cons256" "emacs"))
(define history-max-length 100)
(define history '())
(define original-termios (make-termios 0 0 0 0 (make-vector NCCS 0) 0 0))
(define raw-mode #f)
(define multi-line-mode #f)
(define at-exit-handler #f)
(define completion-callback (lambda (stub) '()))

(struct le-state (in
                  out
                  buffer
                  prompt
                  pos
                  old-pos
                  len
                  columns
                  rows
                  history-index)
        #:mutable)

(define (line-editor prompt [reader read-line])
  (file-stream-buffer-mode stdin 'none)
  (file-stream-buffer-mode stdout 'none)
  (cond
   [(and (supported-terminal?) (terminal-port? stdin)
         (enable-raw-mode STDIN_FILENO))
    (define out (edit prompt stdin))
    (disable-raw-mode STDIN_FILENO)
    (newline)
    out]
   [else (display prompt)
         (flush-output)
         (reader)]))

(define (set-completion-callback callback)
  (set! completion-callback callback))

(define (history-add line)
  (define new-history (cons line history))
  (set! history
        (if ((length new-history) . > . history-max-length)
            (take new-history history-max-length)
            new-history)))

(define (history-set-max-length new-length)
  (set! history-max-length new-length)
  (when ((length history) . > . history-max-length)
    (set! history (take history history-max-length))))

(define (history-save file)
  (for ([i history])
    (displayln i file)))

(define (history-load file)
  (file->lines file))

(define (clear-screen)
  (display "\x1b[H\x1b[2J"))

(define (set-multi-line-mode multi-line)
  (set! multi-line-mode multi-line))

(define (supported-terminal?)
  (define terminal (getenv "TERM"))
  (and terminal
       (not (set-member? unsupported-terminals terminal))))

(define (enable-raw-mode [file STDIN_FILENO])
  (let/ec return
    ;;(unless (terminal-port? (current-input-port)) (return #f))
    (when (= (isatty file) 0) (return #f))
    (unless at-exit-handler
      (set! at-exit-handler
            (plumber-add-flush! (current-plumber)
                                (lambda (x) (disable-raw-mode file)))))
    (when (= (tcgetattr file original-termios) -1) (return #f))
    (define raw
      (make-termios
       (bitwise-and (bitwise-not (bitwise-ior BRKINT ICRNL INPCK ISTRIP IXON))
                    (termios-c_iflag original-termios))
       (bitwise-and (bitwise-not (bitwise-ior OPOST))
                    (termios-c_oflag original-termios))
       (bitwise-ior CS8 (termios-c_cflag original-termios))
       (bitwise-and (bitwise-not (bitwise-ior ECHO ICANON IEXTEN ISIG))
                    (termios-c_lflag original-termios))
       (build-vector NCCS (lambda (n) (cond [(equal? n VMIN) 1]
                                            [(equal? n VTIME) 0]
                                            [else 0])))
       0 0))
    (unless (= (tcsetattr file TCSAFLUSH raw) 0) (return #f))
    (set! raw-mode #t)
    #t))

(define (disable-raw-mode [file STDIN_FILENO])
  (when (and raw-mode (tcsetattr file TCSAFLUSH original-termios))
    (set! raw-mode #f)))

(define (get-columns [in stdin] [out stdout])
  (let/ec return
    (define ws (make-winsize 0 0 0 0))
    (cond
     [(or (= (ioctl 1 TIOCGWINSZ ws) -1)
          (= (winsize-ws_col ws) 0))
      ;; Initial position, to be restored later
      (define start (get-cursor-position in out))
      (unless start (return 80))

      ;; Go to right margin
      (display "\x1b[999C" out)
      (define cols (get-cursor-position in out))
      (unless cols (return 80))

      ;; Restore potion
      (fprintf out "\x1b[~aD" (- (second cols) (second start)))
      cols]
     [else (winsize-ws_col ws)])))

(define (get-cursor-position [in stdin] [out stdout])
  (display "\x1b[6n" out)
  (define pos (read-response in))
  (cond
   [(eof-object? pos) #f]
   [(and (equal? (string-ref pos 0) #\u001B)
         (equal? (string-ref pos 1) #\[)
         (equal? (string-ref pos (- (string-length pos) 1)) #\R))
    (define pos* (substring pos 2 (- (string-length pos) 1)))
    (string-split pos* ";")]
   [else #f]))

(define (complete-line! state)
  (define completions (completion-callback))
  (cond
   [(null? completions) (display "\x7" stderr)] ; Beep
   [else
    (let loop ([i 0])
      (define item
        (cond [(i . < . (length completions)) (list-ref completions i)]
              [else (display "\x7" stderr)
                    #f]))
      (cond [item
             (define saved (struct-copy le-state state))
             (set-le-state-len! state (string-length item))
             (set-le-state-pos! state (string-length item))
             (set-le-state-buffer! state item)
             (refresh-line! state)
             (set-le-state-len! state (le-state-len saved))
             (set-le-state-pos! state (le-state-pos saved))
             (set-le-state-buffer! state (le-state-buffer saved))]
            [else
             (refresh-line! state)])
      (match (read-char (le-state-in state))
        [#\tab (loop (modulo (+ i 1) (+ (length completions) 1)))]
        [#\u1b (when item (refresh-line! state))]                ; escape
        [else
         (when item
           (set-le-state-buffer! state item)
           (set-le-state-len! state (string-length item))
           (set-le-state-pos! state (string-length item)))])
      (string-ref item (- (string-length item) 1)))]))

(define (refresh-line! state)
  (define out (le-state-out state))
  (define prompt (le-state-prompt state))
  (define buffer (le-state-buffer state))
  (define columns (le-state-columns state))
  (define old-rows (le-state-rows state))
  (define pos (le-state-pos state))
  (define old-pos (le-state-old-pos state))
  (define len (le-state-len state))
  (cond
   [multi-line-mode
    (define rows (floor (/ (+ (string-length prompt) columns -1) columns)))
    (define rpos (floor (/ (+ (string-length prompt) old-pos columns -1) columns)))

    ;; Go to the last row
    (when (rows . > . rpos)
      (fprintf out "\x1b[~aB" (- rows rpos)))

    ;; Clear every row
    (for ([j (in-range (- old-rows 1))])
      (display "\r\x1b[0K\x1b[1A" out))

    ;; Clear the top line
    (display "\r\x1b[0K")

    ;; Write prompt and current buffer
    (fprintf out "~a~a" prompt buffer)

    ;; If at end of screen, emit newline
    (when (and (equal? pos len)
               (= (modulo (+ pos (string-length prompt)) columns) 0))
      (display "\n\r" out)
      (set! rows (+ rows 1)))

    ;; Move cursor to correct position
    (define rpos* (floor (/ (+ (string-length prompt) pos columns) columns)))
    (when (rows . > . rpos*)
      (fprintf out "\x1b[~aA" (- rows rpos*)))
    (define col (modulo (+ (string-length prompt) pos) columns))
    (if (col . > . 0)
        (fprintf out "\r\x1b[~aC" col)
        (display "\r" out))

    (set-le-state-old-pos! state pos)
    (when (rows . > . old-rows) (set-le-state-rows! state rows))]

   [else ; Go to left, print prompt and buffer, clear rest of line
    (fprintf out "\r~a~a\x1b[0K\r\x1b[~aC"
             prompt buffer (+ (string-length prompt) pos))]))

(define (edit prompt [in stdin] [out stdout])
  (let/ec return
    (define state (le-state in out "" prompt 0 0 0 (get-columns in out) 0 0))
    (history-add "")
    (display prompt out)
    (let loop ()
      (define c (read-char in))
      (when (eof-object? c) (return (le-state-len state)))
      (when (equal? c #\tab)
        (set! c (complete-line! state)))
      (match c
        [#\ud                                            ; enter
         (set! history (take history (- (length history) 1)))
         (when multi-line-mode (edit-move-end! state))]
        [#\u3 (exit 130)]                                ; ctrl-c
        [(or #\u8 #\u7f) (edit-backspace! state) (loop)] ; ctr-h or backspace
        [#\u4                                            ; ctrl-d
         (cond [((le-state-len state) . > . 0) (edit-delete! state) (loop)]
               [else (set! history (take history (- (length history) 1)))
                     eof])]
        [#\u14                                           ; ctrl-t
         (when (and ((le-state-pos state) . > . 0)
                    ((le-state-pos state) . < . (le-state-len state)))
           (define str (le-state-buffer state))
           (define pos (le-state-pos state))
           (define aux (string-ref str (- pos 1)))
           (string-set! str (- pos 1) (string-ref str pos))
           (string-set! str pos aux)
           (when (not (= pos (- (le-state-len state) 1)))
             (set-le-state-pos! state (+ pos 1)))
           (refresh-line! state))
         (loop)]
        [#\u2 (edit-move-left! state) (loop)]            ; ctrl-b
        [#\u6 (edit-move-right! state) (loop)]           ; ctrl-f
        [#\u10 (edit-history-next! state 'prev) (loop)]   ; ctrl-p
        [#\ue (edit-history-next! state 'next) (loop)]    ; ctrl-n
        [#\u1 (edit-move-home! state) (loop)]            ; ctrl-a
        [#\u5 (edit-move-end! state) (loop)]             ; ctrl-e
        [#\u1b ; escape
         (define a (read-char))
         (define b (read-char))
         (define c (and (equal? a #\[) (b . char>=? . #\0) (b . char<=? . #\9)
                        (read-char)))
         (define d (and (equal? a #\[) (b . char>=? . #\0) (b . char<=? . #\9)
                        (equal? c #\;) (read-char)))
         (define e (and (equal? a #\[) (b . char>=? . #\0) (b . char<=? . #\9)
                        (equal? c #\;) (read-char)))
         (match* (a b c d e)
           [(#\[ #\3 #\~ _ _) (edit-delete! state)]           ; delete
           [(#\[ #\A _ _ _) (edit-history-next! state 'prev)] ; up
           [(#\[ #\B _ _ _) (edit-history-next! state 'next)] ; down
           [(#\[ #\1 #\; #\2 #\A) (edit-history-next! state 'prev)] ; up
           [(#\[ #\1 #\; #\2 #\B) (edit-history-next! state 'next)] ; down
           [(#\[ #\C _ _ _) (edit-move-right! state)]         ; right
           [(#\[ #\D _ _ _) (edit-move-left! state)]          ; left
           [(#\[ #\1 #\; #\2 #\C) (edit-move-right! state)]         ; right
           [(#\[ #\1 #\; #\2 #\D) (edit-move-left! state)]          ; left
           [(#\[ #\H _ _ _) (edit-move-home! state)]          ; home
           [(#\[ #\F _ _ _) (edit-move-end! state)]           ; end
           [(#\O #\H _ _ _) (edit-move-home! state)]          ; home
           [(#\O #\F _ _ _) (edit-move-end! state)]           ; state
           [(_ _ _ _ _)     (void)])                          ; not supported
         (loop)]
        [else (edit-insert! state c) (loop)]))
    (le-state-buffer state)))

(define (edit-move-home! state)
  (unless (= (le-state-pos state) 0)
    (set-le-state-pos! state 0)
    (refresh-line! state)))

(define (edit-move-end! state)
  (unless (= (le-state-pos state) (le-state-len state))
    (set-le-state-pos! state (le-state-len state))
    (refresh-line! state)))

(define (edit-backspace! state)
  (when (and ((le-state-pos state) . > . 0) ((le-state-len state) . > . 0))
    (set-le-state-pos! state (- (le-state-pos state) 1))
    (set-le-state-len! state (- (le-state-len state) 1))
    (let ([pos (le-state-pos state)]
          [buf (le-state-buffer state)])
      (set-le-state-buffer! state (string-append
                                   (substring buf 0 pos)
                                   (substring buf (+ pos 1) (string-length buf)))))
    (refresh-line! state)))

(define (edit-delete! state)
  (when (and ((le-state-pos state) . > . 0)
             ((le-state-len state) . > . (le-state-pos state)))
    (set-le-state-len! state (- (le-state-len state) 1))
    (let ([pos (le-state-pos state)]
          [buf (le-state-buffer state)])
      (set-le-state-buffer! state (string-append
                                   (substring buf 0 pos)
                                   (substring buf (+ pos 1) (string-length buf)))))
    (refresh-line! state)))

(define (edit-move-left! state)
  (when ((le-state-pos state) . > . 0)
    (set-le-state-pos! state (- (le-state-pos state) 1))
    (refresh-line! state)))

(define (edit-move-right! state)
  (when (not (= (le-state-pos state) (le-state-len state)))
    (set-le-state-pos! state (+ (le-state-pos state) 1))
    (refresh-line! state)))

(define (edit-history-next! state direction)
  (define h-index (le-state-history-index state))
  (set! history `(,@(take history h-index)
                  ,(le-state-buffer state)
                  ,@(drop history (+ h-index 1))))
  (define new-index (+ (le-state-history-index state)
                       (match direction ['next 1] ['prev -1])))
  (cond
   [(new-index . < . 0) (set-le-state-history-index! state 0)]
   [(new-index . >= . (length history))
    (set-le-state-history-index! state (- (length history) 1))]
   [else
    (set-le-state-history-index! state new-index)
    (define new-text (list-ref history new-index))
    (set-le-state-buffer! state new-text)
    (set-le-state-len! state (string-length new-text))
    (set-le-state-pos! state (string-length new-text))
    (refresh-line! state)]))

(define (edit-insert! state character)
  (define len (le-state-len state))
  (define pos (le-state-pos state))
  (define buf (le-state-buffer state))
  (cond
   [(= len pos)
    (set-le-state-buffer! state (string-append buf (string character)))
    (set-le-state-pos! state (+ pos 1))
    (set-le-state-len! state (+ len 1))
    (if (and (not multi-line-mode)
             ((string-length (le-state-prompt state)) . < . (le-state-columns state)))
        (display character (le-state-out state))
        (refresh-line! state))]
   [else
    (set-le-state-buffer! state (string-append
                                 (substring buf 0 pos)
                                 (string character)
                                 (substring buf pos (string-length buf))))
    (set-le-state-pos! state (+ pos 1))
    (set-le-state-len! state (+ len 1))
    (refresh-line! state)]))

(module+ test
  (check-true ; Get columns
   (< 0
      (let ()
        (file-stream-buffer-mode (current-input-port) 'none)
        (file-stream-buffer-mode (current-output-port) 'none)
        (void (enable-raw-mode))
        (define cols (get-columns))
        (void (disable-raw-mode))
        cols)))
  (check-true ; Get cursor position
   (pair?
    (let ()
      (file-stream-buffer-mode (current-input-port) 'none)
      (file-stream-buffer-mode (current-output-port) 'none)
      (void (enable-raw-mode))
      (define x (get-cursor-position))
      (void (disable-raw-mode))
      x)))
  (check-equal? ; Display prompt
   (call-with-output-string
    (lambda (stdout)
      (file-stream-buffer-mode stdin 'none)
      (void (enable-raw-mode))
      (define state (le-state stdin stdout "" "foo>" 0 0 0 (get-columns stdin) 0 0))
      (refresh-line! state)
      (void (disable-raw-mode))))
   "\rfoo>\e[0K\r\e[4C")
  (check-equal? ; type characters
   (call-with-output-string
    (lambda (stdout)
      (file-stream-buffer-mode stdin 'none)
      (void (enable-raw-mode))
      (define state (le-state stdin stdout "" "foo>" 0 0 0 (get-columns stdin) 0 0))
      (edit-insert! state #\h)
      (edit-insert! state #\e)
      (edit-insert! state #\l)
      (edit-insert! state #\l)
      (edit-insert! state #\o)
      (refresh-line! state)
      (void (disable-raw-mode))))
   "hello\rfoo>hello\e[0K\r\e[9C")
  (check-equal? ; Backspace
   (call-with-output-string
    (lambda (stdout)
      (file-stream-buffer-mode stdin 'none)
      (void (enable-raw-mode))
      (define state (le-state stdin stdout "" "foo>" 0 0 0 (get-columns stdin) 0 0))
      (edit-insert! state #\y)
      (edit-insert! state #\e)
      (edit-backspace! state)
      (edit-backspace! state)
      (edit-insert! state #\n)
      (edit-insert! state #\o)
      (refresh-line! state)
      (void (disable-raw-mode))))
   "ye\rfoo>y\e[0K\r\e[5C\rfoo>\e[0K\r\e[4Cno\rfoo>no\e[0K\r\e[6C")
  (check-equal? ; left arrow key
   (call-with-output-string
    (lambda (stdout)
      (file-stream-buffer-mode stdin 'none)
      (void (enable-raw-mode))
      (define state (le-state stdin stdout "" "foo>" 0 0 0 (get-columns stdin) 0 0))
      (edit-insert! state #\y)
      (edit-insert! state #\s)
      (edit-move-left! state)
      (edit-insert! state #\e)
      (refresh-line! state)
      (void (disable-raw-mode))))
   "ys\rfoo>ys\e[0K\r\e[5C\rfoo>yes\e[0K\r\e[6C\rfoo>yes\e[0K\r\e[6C")
  (check-equal? ; cat
   (call-with-output-string
    (lambda (stdout)
      (file-stream-buffer-mode stdin 'none)
      (void (enable-raw-mode))
      (define state (le-state stdin stdout "" "foo>" 0 0 0 (get-columns stdin) 0 0))
      (edit-insert! state #\o)
      (edit-move-left! state)
      (edit-insert! state #\a)
      (edit-delete! state)
      (edit-move-left! state)
      (edit-insert! state #\c)
      (edit-move-right! state)
      (edit-insert! state #\t)
      (refresh-line! state)
      (void (disable-raw-mode))))
   "o\rfoo>o\e[0K\r\e[4C\rfoo>ao\e[0K\r\e[5C\rfoo>a\e[0K\r\e[5C\rfoo>a\e[0K\r\e[4C\rfoo>ca\e[0K\r\e[5C\rfoo>ca\e[0K\r\e[6Ct\rfoo>cat\e[0K\r\e[7C")
   )

