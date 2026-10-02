#lang racket/base

;; Stand-in for OpenSSH: record argv, then execute the quoted remote command
;; through a shell just as ssh would, without opening a network connection.
(module+ main
  (require racket/system racket/string)
  (define args (vector->list (current-command-line-arguments)))
  (call-with-output-file (getenv "REMOTE_SHELL_TEST_ARGV")
    #:exists 'truncate/replace
    (lambda (out) (write args out)))
  (case (string->symbol (getenv "REMOTE_SHELL_TEST_MODE"))
    [(sleep) (sleep 60)]
    [(fail) (eprintf "connection failed\n") (exit 1)]
    [(execute)
     (define command
       (let loop ([args args])
         (cond
           [(member (car args) '("-o" "-i" "-R")) (loop (cddr args))]
           [else (cdr args)])))
     (exit (if (system* "/bin/sh" "-c" (string-join command " ")) 0 1))]
    [else (void)]))
