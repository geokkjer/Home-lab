;; utils/ssh.scm - SSH operations facade (aggregates modular components)

(define-module (utils ssh)
  #:use-module (utils ssh connection-test)
  #:use-module (utils ssh remote-command)
  #:use-module (utils ssh file-copy)
  #:use-module (utils ssh retry)
  #:use-module (utils ssh context)
  #:re-export (test-ssh-connection
               run-remote-command
               run-remote-command-pure
               copy-file-to-remote
               copy-file-pure
               run-command-with-retry
               with-retry
               with-ssh-connection))

;; This module acts as a facade, re-exporting functions from specialized modules
;; Each sub-module follows the single responsibility principle:
;; - connection-test: SSH connectivity testing
;; - remote-command: Command execution on remote machines
;; - file-copy: File transfer operations
;; - retry: Retry logic and error recovery
;; - context: Connection context management
;;
;; Pure functions are exported alongside their impure wrappers,
;; allowing callers to choose the appropriate level of abstraction.
