#lang racket/base

(provide set-multi-line-mode)

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

(define-cstruct _winsize ([ws_row    _ushort]
                          [ws_col    _ushort]
                          [ws_xpixel _ushort]
                          [ws_ypixel _ushort]))
(define (new-winsize) (make-winsize 0 0 0 0))

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
(define unsupported-terminals (set "dumb" "cons256" "emacs"))
(define history-max-length 100)
(define history '())
(define original-termios (new-termios))
(define raw-mode #f)
(define multi-line-mode #f)
(define at-exit-handler #f)
(define completion-callback (lambda (str completions) (void)))

(struct line-edit-state (input-file
                         output-file
                         buffer
                         prompt
                         cursor-position
                         previous-cursor-position
                         length
                         columns
                         max-rows
                         history-index))

(define (line-editor prompt [reader read])
  (cond
   [(and (supported-terminal?) (terminal-port? stdin)
         (enable-raw-mode STDIN_FILENO))
    (define out (edit prompt STDIN_FILENO))
    (disable-raw-mode STDIN_FILENO)
    (newline)
    out]
   [else (display prompt)
         (flush-output)
         (reader)]))

(define (set-completion-callback callback)
  (set! completion-callback callback))

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

(define (get-columns [in stdin] [out stdout])
  (let/ec return
    (define ws (new-winsize))
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
      (display (format "\x1b[~aD" (- cols start)))
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

(define (complete-line state)
  (void))

(define (edit prompt [in stdin] [out stdout])
  (define state (line-edit-state in out "" prompt 0 0 0 0
                                 (get-columns in) 0 0))
  (history-add "")
  (display prompt out)
  (let/ec return
    (let loop ()
      (define c (read-char in))
      (when (eof-object? c) (return (line-edit-state-length state)))
      (when (equal? c #\tab)
        (void)))))

(module+ test
  (check-true
   (< 0
      (let ()
        (file-stream-buffer-mode (current-input-port) 'none)
        (file-stream-buffer-mode (current-output-port) 'none)
        (void (enable-raw-mode))
        (define cols (get-columns))
        (void (disable-raw-mode))
        cols))
   (check-true
    (pair?
     (let ()
       (file-stream-buffer-mode (current-input-port) 'none)
       (file-stream-buffer-mode (current-output-port) 'none)
       (void (enable-raw-mode))
       (define x (get-cursor-position))
       (void (disable-raw-mode))
       x)))))
