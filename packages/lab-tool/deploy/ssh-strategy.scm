;; deploy/ssh-strategy.scm - Pure SSH deployment strategy

(define-module (deploy ssh-strategy)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (core config)
  #:use-module (core commands)
  #:export (build-ssh-deploy-commands
            build-rsync-command
            build-nixos-rebuild-command
            build-ssh-key-check-command
            get-deploy-options))

;; Pure function to get deploy options with defaults
(define (get-deploy-options options)
  "Extract deployment options with sensible defaults"
  `((dry-run . ,(or (assoc-ref options 'dry-run) #f))
    (boot . ,(or (assoc-ref options 'boot) #f))
    (test . ,(or (assoc-ref options 'test) #f))
    (switch . ,(or (assoc-ref options 'switch) #f))
    (timeout . ,(or (assoc-ref options 'timeout) 300))))

;; Pure function to build SSH key check command
(define (build-ssh-key-check-command host-config)
  "Build SSH key check command to verify connectivity"
  (let ((hostname (assoc-ref host-config 'hostname))
        (user (assoc-ref host-config 'user))
        (ssh-key (get-ssh-key)))
    (format #f "ssh -i ~a -o BatchMode=yes -o ConnectTimeout=5 ~a@~a 'echo \"SSH key check successful\"'" 
            ssh-key user hostname)))

;; Pure function to build rsync command
(define (build-rsync-command flake-path host-config)
  "Build rsync command to sync flake to remote host using /tmp"
  (let ((hostname (assoc-ref host-config 'hostname))
        (user (assoc-ref host-config 'user))
        (ssh-key (get-ssh-key)))
    (format #f "rsync -av --delete -e 'ssh -i ~a -o BatchMode=yes' ~a/ ~a@~a:/tmp/flake/" 
            ssh-key flake-path user hostname)))

;; Pure function to build nixos-rebuild command
(define (build-nixos-rebuild-command hostname options)
  "Build nixos-rebuild command for remote execution using /tmp/flake"
  (let ((mode (cond
               ((assoc-ref options 'dry-run) "dry-run")
               ((assoc-ref options 'boot) "boot")
               ((assoc-ref options 'test) "test")
               ((assoc-ref options 'switch) "switch")
               (else "switch"))))
    (format #f "sudo nixos-rebuild ~a --flake /tmp/flake#~a" mode hostname)))

;; Pure function to build complete SSH deployment commands
(define (build-ssh-deploy-commands host-name options)
  "Build all commands needed for SSH deployment strategy"
  (let* ((host-config (get-host-config host-name))
         (flake-path (get-flake-path))
         (deploy-opts (get-deploy-options options)))
    (if host-config
        (let* ((hostname (assoc-ref host-config 'hostname))
               (user (get-config-value '(ssh-user) "sma"))
               (ssh-config `((hostname . ,hostname)
                            (user . ,user)))
               (ssh-check-cmd (build-ssh-key-check-command ssh-config))
               (rsync-cmd (build-rsync-command flake-path ssh-config))
               (rebuild-cmd (build-nixos-rebuild-command hostname deploy-opts)))
          `((ssh-check . ,ssh-check-cmd)
            (rsync . ,rsync-cmd)
            (rebuild . ,rebuild-cmd)
            (ssh-config . ,ssh-config)
            (options . ,deploy-opts)))
        #f)))
