#!/usr/bin/env guile
!#

;; Home Lab Tool - Guile Scheme Implementation (Minimal Version)
;; Main entry point for the lab command-line tool

(use-modules (ice-9 match)
             (ice-9 format))

;; Simple logging
(define (log-info msg . args)
  (apply format #t (string-append "[lab] " msg "~%") args))

(define (log-error msg . args)
  (apply format (current-error-port) (string-append "[ERROR] " msg "~%") args))

;; Configuration
(define machines '("congenital-optimist" "sleeper-service" "grey-area" "reverse-proxy"))

;; Main command dispatcher
(define (dispatch-command command args)
  (match command
    ("status" 
     (log-info "Infrastructure status:")
     (for-each (lambda (machine)
                 (format #t "  ~a: Online~%" machine))
               machines))
    
    ("deploy" 
     (if (null? args)
         (log-error "deploy command requires machine name")
         (let ((machine (car args)))
           (if (member machine machines)
               (log-info "Deploying to ~a..." machine)
               (log-error "Unknown machine: ~a" machine)))))
    
    ("mcp"
     (if (null? args)
         (log-error "mcp command requires: start, stop, or status")
         (match (car args)
           ("status" (log-info "MCP server: Development mode"))
           (_ (log-error "MCP command not implemented: ~a" (car args))))))
    
    (_ (log-error "Unknown command: ~a" command))))

;; Show help
(define (show-help)
  (format #t "Home Lab Tool (Guile) v0.1.0

Usage: lab [COMMAND] [ARGS...]

Commands:
  status                Show infrastructure status
  deploy MACHINE        Deploy to machine
  mcp status           Show MCP server status
  help                 Show this help

Machines: ~a
" (string-join machines ", ")))

;; Main entry point
(define (main args)
  (if (< (length args) 2)
      (show-help)
      (let ((command (cadr args))
            (command-args (cddr args)))
        (if (string=? command "help")
            (show-help)
            (dispatch-command command command-args)))))

;; Execute main if this script is run directly
(when (and (> (length (command-line)) 0)
           (string=? (car (command-line)) "./home-lab-tool.scm"))
  (main (command-line)))