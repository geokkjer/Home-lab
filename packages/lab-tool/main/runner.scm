;; main/runner.scm - Main entrypoint for lab-tool

(define-module (main runner)
  #:use-module (core config)
  #:use-module (core commands)
  #:use-module (deploy ssh-strategy)
  #:use-module (deploy executor)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (main))

;; Helper function to join strings
(define (string-join strings separator)
  "Join a list of strings with separator"
  (if (null? strings)
      ""
      (fold (lambda (str acc)
              (if (string=? acc "")
                  str
                  (string-append acc separator str)))
            ""
            strings)))

;; Parse command line arguments
(define (parse-args args)
  "Parse command line arguments into command and options"
  (if (< (length args) 2)
      '(help)
      (let ((command (cadr args))
            (remaining (cddr args)))
        (case (string->symbol command)
          ((update) `(update))
          ((deploy) 
           (if (null? remaining)
               '(deploy-help)
               `(deploy ,(car remaining) ,@(cdr remaining))))
          ((list) `(list))
          ((help) `(help))
          (else `(help))))))

;; Handle update command
(define (handle-update)
  "Handle flake update command"
  (display "Updating flake inputs...\n")
  (display (build-flake-update-command))
  (newline))

;; Handle deploy command
(define (handle-deploy host-name . options)
  "Handle deployment to a specific host"
  (if (validate-host-name host-name)
      (let* ((deploy-options '()) ; TODO: Parse options from command line
             (result (deploy-to-machine host-name deploy-options)))
        (if result
            (display "Deployment completed successfully!\n")
            (display "Deployment failed!\n")))
      (begin
        (display (format #f "Invalid host name: ~a\n" host-name))
        (display "Valid hosts: ")
        (display (string-join (get-all-hosts) ", "))
        (newline)
        #f)))

;; Handle list command
(define (handle-list)
  "Handle list hosts command"
  (list-available-machines))

;; Show help
(define (show-help)
  "Show help message"
  (display "Usage: lab-tool <command> [options]\n")
  (display "\nCommands:\n")
  (display "  update          Update flake inputs\n")
  (display "  deploy <host>   Deploy to specified host\n")
  (display "  list            List all hosts\n")
  (display "  help            Show this help message\n")
  (display "\nHosts:\n")
  (for-each (lambda (host)
              (display "  ")
              (display host)
              (newline))
            (get-all-hosts)))

;; Simple functional main: handles commands
(define (main)
  (let* ((args (command-line))
         (parsed (parse-args args)))
    (case (car parsed)
      ((update) (handle-update))
      ((deploy) (apply handle-deploy (cdr parsed)))
      ((list) (handle-list))
      ((deploy-help) 
       (display "Usage: lab-tool deploy <host> [options]\n")
       (show-help))
      (else (show-help)))))
