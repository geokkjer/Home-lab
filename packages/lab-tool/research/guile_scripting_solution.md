# Replacing Bash with Guile Scheme for Home Lab Tools

This document outlines a proposal to migrate the `home-lab-tools` script from Bash to GNU Guile Scheme. This change aims to address the increasing complexity of the script and leverage the benefits of a more powerful programming language.

## 1. Introduction: Why Guile Scheme?

GNU Guile is the official extension language for the GNU Project. It is an implementation of the Scheme programming language, a dialect of Lisp. Using Guile for scripting offers several advantages over Bash, especially as scripts grow in size and complexity.

Key reasons for considering Guile:

* **Expressiveness and Power:** Scheme is a full-fledged programming language with features like first-class functions, macros, and a rich standard library. This allows for more elegant and maintainable solutions to complex problems.
* **Better Error Handling:** Guile provides robust error handling mechanisms (conditions and handlers) that are more sophisticated than Bash's `set -e` and trap.
* **Modularity:** Guile supports modules, making it easier to organize code into reusable components.
* **Data Manipulation:** Scheme excels at handling structured data, which can be beneficial for managing configurations or parsing output from commands.
* **Readability (for Lisp programmers):** While Lisp syntax can be initially unfamiliar, it can lead to very clear and concise code once learned.
* **Interoperability:** Guile can easily call external programs and libraries, and can be extended with C code if needed.

## 2. Advantages over Bash for `home-lab-tools`

Migrating `home-lab-tools` from Bash to Guile offers specific benefits:

* **Improved Logic Handling:** Complex conditional logic, loops, and function definitions are more naturally expressed in Guile. The current Bash script uses case statements and string comparisons extensively, which can become unwieldy.
* **Structured Data Management:** Machine definitions, deployment modes, and status information could be represented as Scheme data structures (lists, association lists, records), making them easier to manage and query.
* **Enhanced Error Reporting:** More descriptive error messages and better control over script termination in case of failures.
* **Code Reusability:** Functions for common tasks (e.g., SSHing to a machine, running `nixos-rebuild`) can be more cleanly defined and reused.
* **Easier Testing:** Guile's nature as a programming language makes it more amenable to unit testing individual functions or modules.
* **Future Extensibility:** Adding new commands, machines, or features will be simpler and less error-prone in a more structured language.

## 3. Setting up Guile

Guile is often available through system package managers. On NixOS, it can be added to your environment or system configuration.

```nix
# Example: Adding Guile to a Nix shell
nix-shell -p guile
```

A Guile script typically starts with a shebang line:

```scheme
#!/usr/bin/env guile
!#
```

The `!#` at the end is a Guile-specific convention that allows the script to be both executable and loadable into a Guile REPL.

## 4. Basic Guile Scripting Concepts

* **S-expressions:** Code is written using S-expressions (Symbolic Expressions), which are lists enclosed in parentheses, e.g., `(function arg1 arg2)`.
* **Definitions:** `(define variable value)` and `(define (function-name arg1 arg2) ...body...)`.
* **Procedures (Functions):** Core of Guile programming.
* **Control Flow:** `(if condition then-expr else-expr)`, `(cond (test1 expr1) (test2 expr2) ... (else else-expr))`, `(case ...)`
* **Modules:** `(use-modules (ice-9 popen))` for using libraries.

## 5. Interacting with the System

Guile provides modules for system interaction:

* **(ice-9 popen):** For running external commands and capturing their output (similar to backticks or `$(...)` in Bash).
  * `open-pipe* command mode`: Opens a pipe to a command.
  * `get-string-all port`: Reads all output from a port.
* **(ice-9 rdelim):** For reading lines from ports.
* **(ice-9 filesys):** For file system operations (checking existence, deleting, etc.).
  * `file-exists? path`
  * `delete-file path`
* **(srfi srfi-1):** List processing utilities.
* **(srfi srfi-26):** `cut` for partial application, useful for creating specialized functions.
* **Environment Variables:** `(getenv "VAR_NAME")`, `(setenv "VAR_NAME" "value")`.

## Example: Running a command**

```scheme
(use-modules (ice-9 popen))

(define (run-command . args)
  (let* ((cmd (string-join args " "))
         (port (open-pipe* cmd OPEN_READ)))
    (let ((output (get-string-all port)))
      (close-pipe port)
      output)))

(display (run-command "echo" "Hello from Guile"))
(newline)
```

## 6. Error Handling

Guile uses a condition system for error handling.

* `catch`: Allows you to catch specific types of errors.
* `throw`: Raises an error.

```scheme
(use-modules (ice-9 exceptions))

(catch #t
  (lambda ()
    (display "Trying something that might fail...
")
    ;; Example: Force an error
    (if #t (error "Something went wrong!"))
    (display "This won't be printed if an error occurs above.
"))
  (lambda (key . args)
    (format (current-error-port) "Caught an error: ~a - Args: ~a
" key args)
    #f)) ; Return value indicating an error was caught
```

For `home-lab-tools`, this means we can provide more specific feedback when a deployment fails or a machine is unreachable.

## 7. Modularity and Code Organization

Guile's module system allows splitting the code into logical units. For `home-lab-tools`, we could have modules for:

* `lab-config`: Machine definitions, paths.
* `lab-deploy`: Functions related to deploying configurations.
* `lab-ssh`: SSH interaction utilities.
* `lab-status`: Functions for checking machine status.
* `lab-utils`: General helper functions, logging.

**Example module structure:**

```scheme
;; file: lab-utils.scm
(define-module (lab utils)
  #:export (log success warn error))

(define blue "[0;34m")
(define nc "[0m")

(define (log msg)
  (format #t "~a[lab]~a ~a
" blue nc msg))
;; ... other logging functions
```

```scheme
;; file: main-lab-script.scm
#!/usr/bin/env guile
!#
(use-modules (lab utils) (ice-9 popen))

(log "Starting lab script...")
;; ... rest of the script
```

## 8. Example: Rewriting a Small Part of `home-lab-tools.nix` (Conceptual)

Let's consider the `log` function and a simplified `deploy_machine` for local deployment.

**Current Bash:**

```bash
BLUE='[0;34m'
NC='[0m' # No Color

log() {
  echo -e "''${BLUE}[lab]''${NC} $1"
}

deploy_machine() {
  local machine="$1"
  # ...
  if [[ "$machine" == "congenital-optimist" ]]; then
    log "Deploying $machine (mode: $mode) locally"
    sudo nixos-rebuild $mode --flake "$HOMELAB_ROOT#$machine"
  fi
  # ...
}
```

**Conceptual Guile Scheme:**

```scheme
;; main-lab-script.scm
#!/usr/bin/env guile
!#

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 pretty-print)
             (ice-9 exceptions)
             (srfi srfi-1)) ;; For list utilities like `string-join`

;; Configuration (could be in a separate module)
(define homelab-root "/home/geir/Home-lab")

;; Color Definitions
(define RED "[0;31m")
(define GREEN "[0;32m")
(define YELLOW "[1;33m")
(define BLUE "[0;34m")
(define NC "[0m")

;; Logging functions
(define (log level-color level-name message)
  (format #t "~a[~a]~a ~a
" level-color level-name NC message))

(define (info . messages)
  (log BLUE "lab" (apply string-append (map (lambda (m) (if (string? m) m (format #f "~s" m))) messages))))

(define (success . messages)
  (log GREEN "lab" (apply string-append (map (lambda (m) (if (string? m) m (format #f "~s" m))) messages))))

(define (warn . messages)
  (log YELLOW "lab" (apply string-append (map (lambda (m) (if (string? m) m (format #f "~s" m))) messages))))

(define (err . messages)
  (log RED "lab" (apply string-append (map (lambda (m) (if (string? m) m (format #f "~s" m))) messages)))
  (exit 1)) ;; Exit on error

;; Function to run shell commands and handle output/errors
(define (run-shell-command . command-parts)
  (let ((command-string (string-join command-parts " ")))
    (info "Executing: " command-string)
    (let ((pipe (open-pipe* command-string OPEN_READ)))
      (let loop ((lines '()))
        (let ((line (read-line pipe)))
          (if (eof-object? line)
              (begin
                (close-pipe pipe)
                (reverse lines)) ;; Return lines in order
              (begin
                (display line) (newline) ;; Display live output
                (loop (cons line lines)))))))
    ;; TODO: Add proper error checking based on exit status of the command
    ;; For now, we assume success if open-pipe* doesn't fail.
    ;; A more robust solution would check `close-pipe` status or use `system*`.
    ))

;; Simplified deploy_machine
(define (deploy-machine machine mode)
  (info "Deploying " machine " (mode: " mode ")")
  (cond
    ((string=? machine "congenital-optimist")
     (info "Deploying " machine " locally")
     (catch #t
       (lambda ()
         (run-shell-command "sudo" "nixos-rebuild" mode "--flake" (string-append homelab-root "#" machine))
         (success "Successfully deployed " machine))
       (lambda (key . args)
         (err "Failed to deploy " machine ". Error: " key " Args: " args))))
    ;; Add other machines here
    (else
     (err "Unknown machine: " machine))))

;; Main script logic (parsing arguments, calling functions)
(define (main args)
  (if (< (length args) 3)
      (begin
        (err "Usage: <script> deploy <machine> [mode]")
        (exit 1))
      (let ((command (cadr args))
            (machine (caddr args))
            (mode (if (> (length args) 3) (cadddr args) "boot")))
        (cond
          ((string=? command "deploy")
           (deploy-machine machine mode))
          ;; Add other commands like "status", "update"
          (else
           (err "Unknown command: " command))))))

;; Run the main function with command-line arguments
;; (cdr args) to skip the script name itself
(main (program-arguments))
```

## 9. Creating Terminal User Interfaces (TUIs) with Guile-Ncurses

For more interactive command-line tools, Guile Scheme can be used to create Text User Interfaces (TUIs). The primary library for this is `guile-ncurses`.

**Guile-Ncurses** is a GNU project that provides Scheme bindings for the ncurses library, including its components for forms, panels, and menus. This allows you to build sophisticated text-based interfaces directly in Guile.

**Key Features:**

* **Windowing:** Create and manage multiple windows on the terminal.
* **Input Handling:** Process keyboard input, including special keys.
* **Text Attributes:** Control colors, bolding, underlining, and other text styles.
* **Forms, Panels, Menus:** Higher-level components for building complex interfaces.

**Getting Started with Guile-Ncurses:**

1. **Installation:** `guile-ncurses` would typically be installed via your system's package manager or built from source. If you are using NixOS, you would look for a Nix package for `guile-ncurses`.

    ```nix
    # Example: Adding guile-ncurses to a Nix shell (package name might vary)
    nix-shell -p guile guile-ncurses
    ```

2. **Using in Code:**
    You would use the `(ncurses curses)` module (and others like `(ncurses form)`, `(ncurses menu)`, `(ncurses panel)`) in your Guile script.

    ```scheme
    (use-modules (ncurses curses))

    (define (tui-main stdscr)
      ;; Initialize ncurses
      (cbreak!)       ;; Line buffering disabled, Pass on ever char
      (noecho!)       ;; Don't echo() while we do getch
      (keypad stdscr #t) ;; Enable Fx keys, arrow keys etc.

      (addstr "Hello, Guile Ncurses TUI!")
      (refresh)
      (getch) ;; Wait for a key press
      (endwin)) ;; End curses mode

    ;; Initialize and run the TUI
    (initscr)
    (tui-main stdscr)
    ```

**Resources:**

* **Guile-Ncurses Project Page:** [https://www.nongnu.org/guile-ncurses/](https://www.nongnu.org/guile-ncurses/)
* **Guile-Ncurses Manual:** [https://www.gnu.org/software/guile-ncurses/manual/](https://www.gnu.org/software/guile-ncurses/manual/)

Integrating `guile-ncurses` can significantly enhance the user experience of your `home-lab-tools` script, allowing for interactive menus, status dashboards, and more complex user interactions beyond simple command-line arguments and output.

## 10. Conclusion and Next Steps

Migrating `home-lab-tools` to Guile Scheme offers a path to a more maintainable, robust, and extensible solution. While there is a learning curve for Scheme, the long-term benefits for managing a complex set of administration tasks are significant.

**Next Steps:**

1. **Install Guile:** Ensure Guile is available in the development environment.
2. **Start Small:** Begin by porting one command or a set of utility functions (e.g., logging, SSH wrappers).
3. **Learn Guile Basics:** Familiarize with Scheme syntax, common procedures, and modules. The Guile Reference Manual is an excellent resource.
4. **Develop Incrementally:** Port functionality piece by piece, testing along the way.
5. **Explore Guile Libraries:** Investigate Guile libraries for argument parsing (e.g., `(gnu cmdline)`), file system operations, and other needs.
6. **Refactor and Organize:** Use Guile's module system to keep the codebase clean and organized.

This transition will require an initial investment in learning and development but promises a more powerful and sustainable tool for managing the home lab infrastructure.
