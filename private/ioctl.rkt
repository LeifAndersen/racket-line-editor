#lang racket/base

(provide ioctl
         make-winsize
         winsize-ws_col
         TIOCGWINSZ)

(require ffi/unsafe
         ffi/unsafe/define)

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

(define-ffi-definer define-libc (ffi-lib "libc" '("6" "5" #f)))
(define interfaces (make-hash))

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
