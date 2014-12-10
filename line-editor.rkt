#lang racket/base

(provide line-editor
         history-add
         history-save
         history-load
         history-max-length
         supported-terminal?
         multi-line-mode
         completion-callback
         current-prompt)

(require racket/set
         racket/list
         racket/vector
         racket/file
         racket/port
         racket/match
         racket/format
         racket/string
         "private/ioctl.rkt"
         "private/tty-raw-extension")

(module+ test (require rackunit))

;; Based on liblinenoise: https://github.com/antirez/linenoise

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

(define history '())
(define history-max-length
  (make-parameter 100 (lambda (new-length)
                        (history-trim (history-max-length))
                        new-length)))
(define unsupported-terminals (set "dumb" "cons256" "emacs"))
(define multi-line-mode (make-parameter #t))
(define completion-callback (make-parameter (lambda (stub) '())))
(define current-prompt (make-parameter "> "))
(define stdin (current-input-port))
(define stdout (current-output-port))
(define stderr (current-error-port))

(define (line-editor [out stdout])
  (define in-fsb (file-stream-buffer-mode stdin))
  (define out-fsb (file-stream-buffer-mode out))
  (cond [(and (supported-terminal?) (terminal-port? stdin)
              (begin (file-stream-buffer-mode stdin 'none)
                     (file-stream-buffer-mode out 'none)
                     (tty-raw!)))
         (define res (edit (current-prompt) stdin))
         (tty-restore!)
         (file-stream-buffer-mode stdin in-fsb)
         (file-stream-buffer-mode out out-fsb)
         res]
   [else (display (current-prompt))
         (flush-output)
         (read-line stdin)]))

(define (history-add line)
  (set! history (cons line history))
  (history-trim))

(define (history-save [file #f])
  (put-preferences '(line-editor-history) (list history) #f file))

(define (history-load [file #f])
  (set! history (get-preference 'line-editor-history (lambda () null)
                                      'timestamp file))
  (history-trim))

(define (history-trim [len (history-max-length)])
  (when ((length history) . > . len) (set! history (take history len))))

(define (supported-terminal?)
  (define terminal (getenv "TERM"))
  (and terminal
       (not (set-member? unsupported-terminals terminal))))

(define (read-response [port stdin])
  (let loop ([acc ""])
    (define x (read-char))
    (cond [(eof-object? x) x]
          [else (define acc* (string-append acc (~a x)))
                (if (equal? x #\R)
                    acc*
                    (loop acc*))])))

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
      (second cols)]
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
    (define split (string-split pos* ";"))
    (for/list ([i split])
      (string->number i))]
   [else #f]))

(define (complete-line! state)
  (define completions ((completion-callback)))
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
   [(multi-line-mode)
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
         (history-add (le-state-buffer state))
         (when (multi-line-mode) (edit-move-end! state))
         (display "\n\r" out)]
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
    (if (and (not (multi-line-mode))
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
  (check-true ; Get Cursor Position
   (let ([cols
          (let ()
            (file-stream-buffer-mode (current-input-port) 'none)
            (file-stream-buffer-mode (current-output-port) 'none)
            (void (tty-raw!))
            (define cols (get-cursor-position))
            (void (tty-restore!))
            cols)])
     (and (pair? cols) (number? (first cols)) (= (length cols) 2)
        (number? (second cols)))))
  (check-true ; Get columns
   (< 0
      (let ()
        (file-stream-buffer-mode (current-input-port) 'none)
        (file-stream-buffer-mode (current-output-port) 'none)
        (void (tty-raw!))
        (define cols (get-columns))
        (void (tty-restore!))
        cols)))
  (check-true ; Get cursor position
   (pair?
    (let ()
      (file-stream-buffer-mode (current-input-port) 'none)
      (file-stream-buffer-mode (current-output-port) 'none)
      (void (tty-raw!))
      (define x (get-cursor-position))
      (void (tty-restore!))
      x)))
  (check-equal? ; Display prompt
   (call-with-output-string
    (lambda (stdout)
      (file-stream-buffer-mode stdin 'none)
      (void (tty-raw!))
      (define state (le-state stdin stdout "" "foo>" 0 0 0 (get-columns stdin) 0 0))
      (refresh-line! state)
      (void (tty-restore!))))
   "\rfoo>\e[0K\r\e[4C")
  (check-equal? ; type characters
   (call-with-output-string
    (lambda (stdout)
      (file-stream-buffer-mode stdin 'none)
      (void (tty-raw!))
      (define state (le-state stdin stdout "" "foo>" 0 0 0 (get-columns stdin) 0 0))
      (edit-insert! state #\h)
      (edit-insert! state #\e)
      (edit-insert! state #\l)
      (edit-insert! state #\l)
      (edit-insert! state #\o)
      (refresh-line! state)
      (void (tty-restore!))))
   "hello\rfoo>hello\e[0K\r\e[9C")
  (check-equal? ; Backspace
   (call-with-output-string
    (lambda (stdout)
      (file-stream-buffer-mode stdin 'none)
      (void (tty-raw!))
      (define state (le-state stdin stdout "" "foo>" 0 0 0 (get-columns stdin) 0 0))
      (edit-insert! state #\y)
      (edit-insert! state #\e)
      (edit-backspace! state)
      (edit-backspace! state)
      (edit-insert! state #\n)
      (edit-insert! state #\o)
      (refresh-line! state)
      (void (tty-restore!))))
   "ye\rfoo>y\e[0K\r\e[5C\rfoo>\e[0K\r\e[4Cno\rfoo>no\e[0K\r\e[6C")
  (check-equal? ; left arrow key
   (call-with-output-string
    (lambda (stdout)
      (file-stream-buffer-mode stdin 'none)
      (void (tty-raw!))
      (define state (le-state stdin stdout "" "foo>" 0 0 0 (get-columns stdin) 0 0))
      (edit-insert! state #\y)
      (edit-insert! state #\s)
      (edit-move-left! state)
      (edit-insert! state #\e)
      (refresh-line! state)
      (void (tty-restore!))))
   "ys\rfoo>ys\e[0K\r\e[5C\rfoo>yes\e[0K\r\e[6C\rfoo>yes\e[0K\r\e[6C")
  (check-equal? ; cat
   (call-with-output-string
    (lambda (stdout)
      (file-stream-buffer-mode stdin 'none)
      (void (tty-raw!))
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
      (void (tty-restore!))))
   "o\rfoo>o\e[0K\r\e[4C\rfoo>ao\e[0K\r\e[5C\rfoo>a\e[0K\r\e[5C\rfoo>a\e[0K\r\e[4C\rfoo>ca\e[0K\r\e[5C\rfoo>ca\e[0K\r\e[6Ct\rfoo>cat\e[0K\r\e[7C")
   )

