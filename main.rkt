#lang racket/base

(provide set-multi-line-mode)

(require racket/set
         racket/list
         racket/vector
         racket/file
         racket/port
         racket/match
         racket/format
         ffi/unsafe
         ffi/unsafe/define)

;; Based on liblinenoise: https://github.com/antirez/linenoise

(module+ test (require rackunit))

(define STDIN_FILENO 0)
(define VMIN 16)
(define VTIME 17)

(define TCSANOW    #x0)
(define TCSADRAIN  #x1)
(define TCSAFLUSH  #x2)
(define TCSASOFT   #x10)

(define _tcflag_t _ulong)
(define IGNBRK      #x1)       ; Input
(define BRKINT      #x2)
(define IGNPAR      #x4)
(define PARMRK      #x8)
(define INPCK       #x10)
(define ISTRIP      #x20)
(define INLCR       #x40)
(define IGNCR       #x80)
(define ICRNL       #x100)
(define IXON        #x200)
(define IXOFF       #x400)
(define IXANY       #x800)
(define IMAXBEL     #x2000)
(define OPOST       #x1)       ; Output
(define ONLCR       #x2)
(define OXTABS      #x4)
(define ONOEOT      #x8)
(define CIGNORE     #x1)       ; Control
(define CS5         #x0)
(define CS6         #x100)
(define CS7         #x200)
(define CS8         #x300)
(define CSTOPB      #x400)
(define CREAD       #x800)
(define PARENB      #x1000)
(define PARODD      #x2000)
(define HUPCL       #x4000)
(define CLOCAL      #x8000)
(define CCTS_OPLOW  #x10000)
(define CRTSCTS     #x30000)
(define CRTS_IFLOW  #x20000)
(define CDTR_IFLOW  #x40000)
(define CDSR_OFLOW  #x80000)
(define CCAR_OFLOW  #x100000)
(define MDMBUF      #x100000)
(define ECHOKE      #x1)       ; Local
(define ECHOE       #x2)
(define ECHOK       #x4)
(define ECHO        #x8)
(define ECHONL      #x10)
(define ECHOPRT     #x20)
(define ECHOCTL     #x40)
(define ISIG        #x80)
(define ICANON      #x100)
(define ALTWERASE   #x200)
(define IEXTEN      #x400)
(define EXTPROC     #x800)
(define TOSTOP      #x400000)
(define FLUSHO      #x800000)
(define NOKERNINFO  #x2000000)
(define NOFLSH      #x8000000)

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
(define (new-termios) (make-termios 0 0 0 0 (make-vector NCCS 0) 0 0))

(define-ffi-definer define-libc (ffi-lib "libc"))

(define-libc tcsetattr (_fun _int _int _termios-pointer -> _int))
(define-libc tcgetattr (_fun _int _termios-pointer -> _int))
(define-libc isatty (_fun _int -> _int))

(define (read-response [port (current-input-port)])
  (let loop ([acc ""])
    (define x (read-char))
    (define acc* (string-append acc (~a x)))
    (if (equal? x #\R)
        acc*
        (loop acc*))))

(define unsupported-terminals (set "dumb" "cons256" "emacs"))
(define history-max-length 100)
(define history '())
(define original-termios (new-termios))
(define raw-mode #f)
(define multi-line-mode #f)
(define at-exit-handler #f)

(struct line-edit-state (input-file
                         buffer
                         prompt
                         cursor-position
                         previous-cursor-position
                         columns
                         max-rows
                         history-index))

(define (line-editor prompt [reader read])
  (cond
   [(and (supported-terminal?) (terminal-port? (current-input-port))
         (enable-raw-mode STDIN_FILENO))
    (define out (edit prompt STDIN_FILENO))
    (disable-raw-mode STDIN_FILENO)
    (newline)
    out]
   [else (display prompt)
         (flush-output)
         (reader)]))

#;(define (set-completion-callback callback)
  ...)

#;(define (add-completion completions str)
  ...)

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

#;(define (print-key-codes)
  ...)

(define (supported-terminal?)
  (define terminal (getenv "TERM"))
  (and terminal
       (set-member? unsupported-terminals) terminal))

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
       (build-vector NCCS (lambda (n) (match n
                                        [VMIN 1]
                                        [VTIME 0]
                                        [else 0])))
       0 0))
    (unless (= (tcsetattr file TCSAFLUSH raw) 0) (return #f))
    (set! raw-mode #t)
    #t))

(define (disable-raw-mode [file STDIN_FILENO])
  (when (and raw-mode (tcsetattr file TCSAFLUSH original-termios))
    (set! raw-mode #f)))

(define (get-columns in-file)
  (void))

(define (edit prompt in-file)
  (line-edit-state in-file "" prompt 0 0 0
                   (get-columns in-file) 0 0)
  (history-add "")
  (display prompt)

  (void))

;;(file-stream-buffer-mode (current-input-port) 'none)
;;(file-stream-buffer-mode (current-output-port) 'none)
(void (enable-raw-mode))
(display "\x1b[6n")
(flush-output)
(define response (read-response))
(void (disable-raw-mode))
response

(module+ test
  ;; Tests to be run with raco test
  )
