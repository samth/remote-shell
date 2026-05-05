#lang racket/base

(require rackunit
         (submod remote-shell/docker for-testing))

;; ========== docker-build-argv ==========

(test-case "docker-build-argv: defaults are minimal"
  (check-equal? (docker-build-argv #:name "img" #:content "ctx"
                            #:platform #f #:dockerfile #f
                            #:build-args (hash) #:buildx? #f
                            #:cache-from null #:cache-to null)
                '("build" "--tag" "img" "--rm" "ctx")))

(test-case "docker-build-argv: --file and --platform appear when supplied"
  (check-equal? (docker-build-argv #:name "img" #:content "ctx"
                            #:platform "linux/amd64"
                            #:dockerfile "docker/Dockerfile.e2e"
                            #:build-args (hash) #:buildx? #f
                            #:cache-from null #:cache-to null)
                '("build" "--tag" "img" "--rm"
                  "--file" "docker/Dockerfile.e2e"
                  "--platform" "linux/amd64"
                  "ctx")))

(test-case "docker-build-argv: --build-arg KEY=VALUE for each entry"
  (define argv
    (docker-build-argv #:name "img" #:content "ctx"
                #:platform #f #:dockerfile #f
                #:build-args (hash "BASE_IMAGE" "ubuntu:24.04"
                                   "INCLUDE_SYSTEM_RACKET" "1")
                #:buildx? #f #:cache-from null #:cache-to null))
  ;; Hash order isn't stable, so check membership rather than position.
  (check-not-false (member "--build-arg" argv) "build-arg flag present")
  (check-not-false (member #"BASE_IMAGE=ubuntu:24.04" argv))
  (check-not-false (member #"INCLUDE_SYSTEM_RACKET=1" argv)))

(test-case "docker-build-argv: buildx? 'load uses buildx --load"
  (check-equal? (docker-build-argv #:name "img" #:content "ctx"
                            #:platform #f #:dockerfile #f
                            #:build-args (hash) #:buildx? 'load
                            #:cache-from null #:cache-to null)
                '("buildx" "build" "--load" "--tag" "img" "--rm" "ctx")))

(test-case "docker-build-argv: buildx? 'push uses buildx --push"
  (check-equal? (docker-build-argv #:name "img" #:content "ctx"
                            #:platform #f #:dockerfile #f
                            #:build-args (hash) #:buildx? 'push
                            #:cache-from null #:cache-to null)
                '("buildx" "build" "--push" "--tag" "img" "--rm" "ctx")))

(test-case "docker-build-argv: cache-from/cache-to require buildx?"
  (check-exn #px"cache-from/cache-to require buildx\\?"
             (lambda ()
               (docker-build-argv #:name "img" #:content "ctx"
                           #:platform #f #:dockerfile #f
                           #:build-args (hash) #:buildx? #f
                           #:cache-from '("type=gha,scope=x")
                           #:cache-to null)))
  (check-exn #px"cache-from/cache-to require buildx\\?"
             (lambda ()
               (docker-build-argv #:name "img" #:content "ctx"
                           #:platform #f #:dockerfile #f
                           #:build-args (hash) #:buildx? #f
                           #:cache-from null
                           #:cache-to '("type=gha,scope=x,mode=max")))))

(test-case "docker-build-argv: cache-from/cache-to flatten with buildx?"
  (check-equal? (docker-build-argv #:name "img" #:content "ctx"
                            #:platform #f #:dockerfile #f
                            #:build-args (hash) #:buildx? 'load
                            #:cache-from '("type=gha,scope=a")
                            #:cache-to '("type=gha,scope=a,mode=max"))
                '("buildx" "build" "--load" "--tag" "img" "--rm"
                  "--cache-from=type=gha,scope=a"
                  "--cache-to=type=gha,scope=a,mode=max"
                  "ctx")))

;; ========== docker-exec-argv ==========

(test-case "docker-exec-argv: defaults pass only command and args"
  (check-equal? (docker-exec-argv #:name "c" #:user #f #:workdir #f
                                  #:command "bash" #:args '("-c" "echo hi"))
                '("container" "exec" "c" "bash" "-c" "echo hi")))

(test-case "docker-exec-argv: #:user becomes --user"
  (check-equal? (docker-exec-argv #:name "c" #:user "1000:1000" #:workdir #f
                                  #:command "id" #:args '())
                '("container" "exec" "--user" "1000:1000" "c" "id")))

(test-case "docker-exec-argv: #:workdir becomes --workdir"
  (check-equal? (docker-exec-argv #:name "c" #:user #f #:workdir "/work"
                                  #:command "pwd" #:args '())
                '("container" "exec" "--workdir" "/work" "c" "pwd")))

(test-case "docker-exec-argv: --user and --workdir compose, before the container name"
  (check-equal? (docker-exec-argv #:name "c" #:user "1000:1000" #:workdir "/work"
                                  #:command "bash" #:args '("script.sh"))
                '("container" "exec"
                  "--user" "1000:1000"
                  "--workdir" "/work"
                  "c" "bash" "script.sh")))
