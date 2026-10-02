#lang racket/base

(require rackunit
         json
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/system)

(define-runtime-path ssh-module "../../remote-shell-lib/ssh.rkt")
(define-runtime-path fake-ssh "private/ssh.rkt")

;; Load in a fresh namespace after setting PATH: executable lookup happens
;; when the library is instantiated. This exercises the public API and the
;; actual subprocess argv instead of replacing the command runner.
(define (call-with-fake-ssh proc)
  (define dir (make-temporary-file "remote-shell-~a" 'directory))
  (define record (build-path dir "argv"))
  (define env (environment-variables-copy (current-environment-variables)))
  (define (quote-shell s)
    (string-append "'" (regexp-replace* #rx"'" s "'\"'\"'") "'"))
  (dynamic-wind
    void
    (lambda ()
      (for ([name '("ssh" "scp")])
        (define exe (build-path dir name))
        (call-with-output-file exe
          (lambda (out)
            (fprintf out "#!/bin/sh\nexec ~a ~a \"$@\"\n"
                     (quote-shell (path->string (find-executable-path "racket")))
                     (quote-shell (path->string fake-ssh)))))
        (file-or-directory-permissions exe #o755))
      (environment-variables-set! env #"PATH"
                                  (bytes-append (path->bytes dir) #":"
                                                (or (environment-variables-ref env #"PATH") #"")))
      (environment-variables-set! env #"REMOTE_SHELL_TEST_ARGV" (path->bytes record))
      (environment-variables-set! env #"REMOTE_SHELL_TEST_MODE" #"record")
      (parameterize ([current-environment-variables env]
                     [current-namespace (make-base-namespace)]
                     [current-input-port (open-input-bytes #"")]
                     [current-output-port (open-output-bytes)]
                     [current-error-port (open-output-bytes)])
        (proc (dynamic-require ssh-module 'remote)
              (dynamic-require ssh-module 'ssh)
              (dynamic-require ssh-module 'scp)
              (lambda () (call-with-input-file record read)))))
    (lambda () (delete-directory/files dir))))

(define options '(("BatchMode" . "yes") ("ConnectTimeout" . "5")))

(test-case "default argv keeps config alias and adds no options"
  (call-with-fake-ssh
   (lambda (remote ssh scp argv)
     (check-true (ssh (remote #:host "config-alias") "echo hello" #:mode 'result))
     (check-equal? (argv) '("config-alias" "'/usr/bin/env'" "'/bin/sh'" "'-c'" "'echo hello'")))))

(test-case "options, identity, and tunnels precede destination; command stays quoted"
  (call-with-fake-ssh
   (lambda (remote ssh scp argv)
     (define r (remote #:host "config-alias" #:user "sam"
                       #:key (string->path "key with spaces")
                       #:remote-tunnels '((1234 . 5678))
                       #:env '(("MESSAGE" . "it's fine"))
                       #:ssh-options
                       (append options '(("ProxyCommand" . "ssh jump -W %h:%p")
                                         ("IdentityFile" . "second key")
                                         ("IdentityFile" . "third key")))))
     (check-true (ssh r "printf '%s' \"$MESSAGE\"" #:mode 'result))
     (check-equal? (take (argv) 15)
                   '("-o" "BatchMode=yes" "-o" "ConnectTimeout=5"
                     "-o" "ProxyCommand=ssh jump -W %h:%p"
                     "-o" "IdentityFile=second key" "-o" "IdentityFile=third key"
                     "-i" "key with spaces" "-R" "1234:localhost:5678" "sam@config-alias"))
     (check-equal? (drop (argv) 15)
                   '("'/usr/bin/env'" "'MESSAGE=it'\"'\"'s fine'" "'/bin/sh'" "'-c'"
                     "'printf '\"'\"'%s'\"'\"' \"$MESSAGE\"'"))
     (check-true (scp r "source with spaces" "config-alias:dest" #:mode 'result))
     (check-equal? (take (argv) 10)
                   '("-o" "BatchMode=yes" "-o" "ConnectTimeout=5"
                     "-o" "ProxyCommand=ssh jump -W %h:%p"
                     "-o" "IdentityFile=second key" "-o" "IdentityFile=third key"))
     (check-equal? (drop (argv) 10)
                   '("-i" "key with spaces" "source with spaces" "config-alias:dest")))))

(test-case "structured option contract and Docker rejection"
  (call-with-fake-ssh
   (lambda (remote ssh scp argv)
     (for ([bad (list '("BatchMode=yes") '(("" . "yes"))
                      '(("BatchMode=yes" . "no")) '(("BatchMode other" . "yes"))
                      '(("ConnectTimeout" . 5)) (list (cons "BatchMode" "yes\0")))])
       (check-exn exn:fail:contract?
                  (lambda () (remote #:host "alias" #:ssh-options bad))))
     (check-exn #rx"SSH options are not supported"
                (lambda () (remote #:host "container" #:kind 'docker #:ssh-options options)))
     (check-not-exn (lambda () (remote #:host "container" #:kind 'docker))))))

(test-case "stdin JSON round-trip and shell quoting through fake SSH"
  (call-with-fake-ssh
   (lambda (remote ssh scp argv)
     (putenv "REMOTE_SHELL_TEST_MODE" "execute")
     (define payload (hasheq 'text "quotes: ' \" and spaces\nλ" 'values '(1 2 3)))
     (define result
       (parameterize ([current-input-port (open-input-bytes (jsexpr->bytes payload))])
         (ssh (remote #:host "config-alias" #:ssh-options options) "cat" #:mode 'output)))
     (check-true (car result))
     (check-equal? (bytes->jsexpr (cdr result)) payload)
     (define quoted
       (ssh (remote #:host "config-alias" #:ssh-options options
                    #:env '(("MESSAGE" . "it's a $value; \"quoted\"")))
            "printf '%s' \"$MESSAGE\"" #:mode 'output))
     (check-equal? quoted '(#t . #"it's a $value; \"quoted\"")))))

(test-case "localhost shortcut still runs locally with SSH options supplied"
  (call-with-fake-ssh
   (lambda (remote ssh scp argv)
     (check-equal? (ssh (remote #:host "localhost" #:ssh-options options)
                       "printf local" #:mode 'output)
                   '(#t . #"local"))
     (check-exn exn:fail:filesystem? argv))))

(test-case "connection failures retain all three result modes"
  (call-with-fake-ssh
   (lambda (remote ssh scp argv)
     (putenv "REMOTE_SHELL_TEST_MODE" "fail")
     (define r (remote #:host "alias" #:ssh-options options))
     (check-false (ssh r "true" #:mode 'result))
     (check-equal? (ssh r "true" #:mode 'output) '(#f . #"connection failed\n"))
     (check-exn #rx"ssh: failed" (lambda () (ssh r "true"))))))

(test-case "overall timeout remains independent of ConnectTimeout"
  (call-with-fake-ssh
   (lambda (remote ssh scp argv)
     (putenv "REMOTE_SHELL_TEST_MODE" "sleep")
     (define r (remote #:host "alias" #:timeout 1 #:ssh-options options))
     (define start (current-inexact-monotonic-milliseconds))
     (check-false (ssh r "sleep 60" #:mode 'result))
     (check-true (< (- (current-inexact-monotonic-milliseconds) start) 10000))
     (check-equal? (take (argv) 4) '("-o" "BatchMode=yes" "-o" "ConnectTimeout=5"))
     (define output (ssh r "sleep 60" #:mode 'output))
     (check-false (car output))
     (check-regexp-match #rx#"Timeout after 1 seconds" (cdr output))
     (check-exn #rx"ssh: failed" (lambda () (ssh r "sleep 60"))))))
