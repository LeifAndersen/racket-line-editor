#lang racket/base

(require "line-editor.rkt"
         racket/list racket/file)

;; --------------------------------------------------------------------------
;; Simple namespace-based completion

;; efficiently convert symbols to byte strings
(define symbol->bstring
  (let ([t (make-weak-hash)])
    (lambda (sym)
      (or (hash-ref t sym #f)
          (let ([bstr (string->bytes/utf-8 (symbol->string sym))])
            (hash-set! t sym bstr)
            bstr)))))

;; get a list of byte strings for current bindings, cache last result
(define get-namespace-bstrings
  (let ([last-syms #f] [last-bstrs #f])
    (lambda ()
      (define syms (namespace-mapped-symbols))
      (unless (equal? syms last-syms)
        (set! last-syms syms)
        (set! last-bstrs (sort (map symbol->bstring syms) bytes<?)))
      last-bstrs)))

(define (namespace-completion pat)
  (let* ([pat (if (string? pat) (string->bytes/utf-8 pat) pat)]
         [pat (regexp-quote pat)]
         [pat (regexp-replace* #px#"(\\w)\\b" pat #"\\1\\\\w*")]
         [pat (byte-pregexp (bytes-append #"^" pat))])
    (filter (lambda (bstr) (regexp-match pat bstr))
            (get-namespace-bstrings))))

(completion-callback namespace-completion)

;; --------------------------------------------------------------------------
;; History management
;; (Note: local-history, and the preference are in reverse order, from
;; the newest to the oldest.)

(history-load)

;; captured now so we don't flush some other output port
(define readline-output-port (current-output-port))
(port-count-lines! readline-output-port)

(define (readline-bytes/hist p force-keep?)
  (when (eq? readline-output-port (current-output-port))
    (define-values [line col pos] (port-next-location readline-output-port))
    (when (and col (positive? col)) (newline readline-output-port)))
  (let ([s (line-editor #:prompt p)]) (history-add s force-keep?) s))

(exit-handler
 (let ([old (exit-handler)])
   (lambda (v) (history-save) (old v))))

;; --------------------------------------------------------------------------
;; An input port that goes through readline

;; readline-prompt can be
;;   #f: no prompt (normal state),
;;   <bytes>: a prompt to use
;;   'space: a prompt has been used, now use spaces instead
;;           (from readline-prompt-spaces)
;; this also controls saving multi-line histories: when the prompt is #f we
;; collect history as usual; otherwise, we accumulate the lines in a chunk (and
;; add them to the history without removing duplicates) and at the beginning of
;; each new chunk (when we read a line with a prompt that is not 'space) we
;; throw away the intermediate history lines that were added and add the whole
;; chunk as one big multiline string.
(provide readline-prompt)
(define readline-prompt (make-parameter #f))

(define-struct readline-state (prompt-spaces multiline-chunk)
  #:mutable)

(define readline-state-cell (make-thread-cell #f))
(define (get-readline-state)
  (or (thread-cell-ref readline-state-cell)
      (let ([state (readline-state #"  " '())])
        (thread-cell-set! readline-state-cell state)
        state)))

(define (do-multiline-chunk state)
  (define chunk (readline-state-multiline-chunk state))
  (when (pair? chunk)
    (history-drop chunk)
    (history-add (apply bytes-append (reverse chunk)) #f)
    (set-readline-state-multiline-chunk! state '())))

(define (readline-bytes/multiline-chunk prompt state)
  (define line (readline-bytes/hist prompt #t))
  (when (and (bytes? line) (not (zero? (bytes-length line))))
    (define c (readline-state-multiline-chunk state))
    (set-readline-state-multiline-chunk!
     state
     (if (pair? c)
       (list* line (readline-state-prompt-spaces state) #"\n\r" c)
       (cons line c))))
  line)

(define (do-one-line state k)
  (define p (readline-prompt))
  (case p
    [(#f) (thread (lambda ()
                    (do-multiline-chunk state)
                    (k (readline-bytes/hist #"" #f))))]
    [(space) (thread
              (lambda ()
                (k (readline-bytes/multiline-chunk
                    (readline-state-prompt-spaces state)
                    state))))]
    [else (readline-prompt 'space) ; use spaces next time
          (thread
           (lambda ()
             (do-multiline-chunk state)
             (unless (= (bytes-length (readline-state-prompt-spaces state))
                        (bytes-length p))
               (set-readline-state-prompt-spaces!
                state
                (make-bytes (bytes-length p) 32)))
             (k (readline-bytes/multiline-chunk p state))))]))

(provide readline-input)
(define readline-input
  (let ([buffer  #f]
        [evt     #f]
        [skip    #f]
        [blen    #f]
        [closed? #f]
        [LF      (bytes-ref #"\n\r" 0)])
    (define (close!) (set! closed? #t) (history-save))
    (define (reader tgt)
      (let loop ()
        (cond [closed? eof]
              [(eof-object? buffer) (set! buffer #f) eof]
              [evt evt]
              [(not buffer)
               (set! evt
                     (wrap-evt
                      (do-one-line
                       (get-readline-state)
                       (lambda (buf)
                         (if (eof-object? buf)
                             (history-save)
                             (begin (set! skip 0)
                                    (set! blen (bytes-length buf))))
                         (set! buffer buf)
                         (set! evt #f)))
                      (lambda (v) 0)))
               evt]
              [else
               ;; copy bytes
               (define tgtlen (bytes-length tgt))
               (define left   (- blen skip))
               (cond [(< tgtlen left) ; not enough target space
                      (let ([end (+ skip tgtlen)])
                        (bytes-copy! tgt 0 buffer skip end)
                        (set! skip end)
                        tgtlen)]
                     [(= tgtlen left) ; enough room for text but no newline
                      (bytes-copy! tgt 0 buffer skip blen)
                      (set! skip blen)
                      left]
                     [else ; enough room for text with newline
                      (bytes-copy! tgt 0 buffer skip blen)
                      (bytes-set! tgt left LF)
                      (set! buffer #f)
                      (add1 left)])])))
    (make-input-port 'readline-input reader #f close!)))

;; --------------------------------------------------------------------------
;; Reading functions

;; a function that can be used for current-prompt-read
(provide read-cmdline-syntax)
(define (read-cmdline-syntax)
  (define prompt (current-prompt))
  (flush-output)
  ;; needs to set `readline-prompt' to get a prompt when reading
  (parameterize ([readline-prompt prompt])
    (define in ((current-get-interaction-input-port)))
    (unless (eq? 'readline-input (object-name in))
      ;; not the readline port -- print the prompt (changing the
      ;; readline-prompt and using read-complete-syntax below should still
      ;; work fine)
      (display prompt) (flush-output))
    (begin0 ((current-read-interaction) (object-name in) in)
      (do-multiline-chunk (get-readline-state)))))
