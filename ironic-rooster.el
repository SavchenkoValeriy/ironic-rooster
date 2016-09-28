;;; ironic-rooster.el --- C/C++ minor mode powered by libclang

;; Copyright (C) 2011-2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>,
;;         Valeriy Savchenko <sinmipt@gmail.com>
;; Version: 0.2.0
;; URL: https://github.com/SavchenkoValeriy/ironic-rooster-mode
;; Compatibility: GNU Emacs 23.x, GNU Emacs 24.x
;; Keywords: c, convenience, tools
;; Package-Requires: ((cl-lib "0.5") (json "1.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file provides `ironic-rooster-mode', a minor mode for C, C++ and Objective-C.
;;
;; Usage:
;;     (add-hook 'c++-mode-hook 'ironic-rooster-mode)
;;     (add-hook 'c-mode-hook 'ironic-rooster-mode)
;;     (add-hook 'objc-mode-hook 'ironic-rooster-mode)
;;
;;     ;; replace the `completion-at-point' and `complete-symbol' bindings in
;;     ;; ironic-rooster-mode's buffers by ironic-rooster-mode's asynchronous function
;;     (defun my-ironic-rooster-mode-hook ()
;;       (define-key ironic-rooster-mode-map [remap completion-at-point]
;;         'ironic-rooster-completion-at-point-async)
;;       (define-key ironic-rooster-mode-map [remap complete-symbol]
;;         'ironic-rooster-completion-at-point-async))
;;     (add-hook 'ironic-rooster-mode-hook 'my-ironic-rooster-mode-hook)
;;
;;     ;; Only needed on Windows
;;     (when (eq system-type 'windows-nt)
;;       (setq w32-pipe-read-delay 0))
;;
;; See also:
;; - https://github.com/Sarcasm/company-ironic-rooster
;; - https://github.com/Sarcasm/ac-ironic-rooster

;;; Code:

(autoload 'ironic-rooster-completion--enter "ironic-rooster-completion")
(autoload 'ironic-rooster-completion--exit "ironic-rooster-completion")

(require 'cl-lib)

(autoload 'find-library-name "find-func")
(autoload 'lm-version "lisp-mnt")


;;
;; Compatibility
;;

(eval-and-compile

  ;; As seen in flycheck/magit
  ;;
  ;; Added in Emacs 24.3 (mirrors/emacs@b335efc3).
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val)))

  ;; Added in Emacs 24.3 (mirrors/emacs@b335efc3).
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being
automatically buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var)))))

  ) ;; eval-and-compile


;;
;; Customizable variables
;;

(defgroup ironic-rooster nil
  "C/C++ minor mode powered by libclang."
  :group 'c)

(defcustom ironic-rooster-lighter " Ironic-Rooster"
  "Text to display in the mode line when ironic-rooster mode is on."
  :type 'string
  :group 'ironic-rooster)

(defcustom ironic-rooster-user-dir (locate-user-emacs-file "ironic-rooster/")
  "Directory containing the Ironic-Rooster generated files.

The slash is expected at the end."
  :type 'directory
  :risky t
  :group 'ironic-rooster)

(defcustom ironic-rooster-supported-major-modes '(c++-mode
                                         c-mode
                                         objc-mode)
  "List of modes known to be compatible with Ironic-Rooster."
  :type '(repeat symbol)
  :group 'ironic-rooster)

;;;###autoload
(defcustom ironic-rooster-additional-clang-options nil
  "Additional command line options to pass down to libclang.

Please, do NOT use this variable to add header search paths, only
additional warnings or compiler options.

These compiler options will be prepended to the command line, in
order to not override the value coming from a compilation
database."
  :type '(repeat string)
  :options '("-Wdocumentation")
  :group 'ironic-rooster)

(defcustom ironic-rooster-lang-compile-option-alist
  '((c++-mode  . "c++")
    (c-mode    . "c")
    (objc-mode . "objective-c"))
  "Alist to decide the language option to used based on the `major-mode'."
  :type '(alist :key-type symbol :value-type string)
  :group 'ironic-rooster)

(defcustom ironic-rooster-cmake-executable "cmake"
  "Name or path of the CMake executable."
  :type 'string
  :group 'ironic-rooster)

(defcustom ironic-rooster-server-source-dir
  (expand-file-name "server" (file-name-directory (find-library-name "ironic-rooster")))
  "Points to the ironic-rooster-server source directory.

This should point to the directory that contains the top-most
CMakeLists.txt used to build the server."
  :type 'directory
  :group 'ironic-rooster)

(defcustom ironic-rooster-server-build-dir nil
  "Build directory for ironic-rooster-server.

If set to nil the default is to create a build directory in
`temporary-file-directory'/build-ironic-rooster-server-`(ironic-rooster-version)'."
  :type 'directory
  :group 'ironic-rooster)

(defcustom ironic-rooster-server-install-prefix ironic-rooster-user-dir
  "Installation prefix used to install ironic-rooster-server.

The ironic-rooster-server executable is expected to be in
`ironic-rooster-server-install-prefix'/bin/."
  :type 'directory
  :group 'ironic-rooster)


;;
;; Public/API variables
;;
;; Non-customizable variables provided by Ironic-Rooster that can be useful to other
;; packages.
;;
;; Note that they shouldn't be modified directly by external packages, just
;; read.
;;

;; TODO: make this variable public when the CDB API stabilizes.
(defvar-local ironic-rooster--compile-options nil
  "Compile options for the current file.

The compile options used by the compiler to build the current
buffer file.")

;; TODO: make this variable public when the CDB API stabilizes.
(defvar-local ironic-rooster--working-directory nil
  "The working directory to pass to libclang, if any.")


;;
;; Internal variables
;;
;; The prefix `ironic-rooster--' is used when something can completely change (or
;; disappear) from one release to the other.
;;
;; -- https://lists.gnu.org/archive/html/emacs-devel/2013-06/msg01129.html

(defconst ironic-rooster--eot "\n;;EOT\n"
  "String sent by the server to signal the end of a response.")


;;
;; Utility functions & macros
;;

(defmacro ironic-rooster--aif (test if-expr &rest else-body)
  (declare (indent 2))
  `(let ((it ,test))
     (if it
         ,if-expr
       (progn ,@else-body))))

(defmacro ironic-rooster--awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test))
     (when it
       (progn ,@body))))

(defun ironic-rooster--assoc-all (key list)
  (delq nil (mapcar #'(lambda (c)
                        (when (equal (car c) key)
                          c))
                    list)))

(defmacro ironic-rooster--without-narrowing (&rest body)
  "Remove the effect of narrowing for the current buffer.

Note: If `save-excursion' is needed for BODY, it should be used
before calling this macro."
  (declare (indent 0) (debug t))
  `(save-restriction
     (widen)
     (progn ,@body)))

(defun ironic-rooster--buffer-size-in-bytes ()
  "Return the buffer size, in bytes."
  (1- (position-bytes (point-max))))

(defun ironic-rooster--read-char-choice (prompt chars)
  "Wrapper around `read-char-choice', available since Emacs 24."
  (setq prompt (concat prompt " [" chars "]: "))
  (if (fboundp 'read-char-choice)
      (read-char-choice prompt chars)
    (setq prompt (propertize prompt 'face 'minibuffer-prompt))
    (let ((cursor-in-echo-area t)
          k)
      (while (not (member k chars))
        (setq k (read-char-exclusive prompt)))
      k)))

(defun ironic-rooster--shorten-path (path)
  "Make PATH as short as possible.

The given path can be considered understandable by human but not
necessary a valid path string to use in code. Its only purpose is
to be displayed to the user."
  (let ((relative (file-relative-name path))
        (abbreviated (abbreviate-file-name path)))
    (if (< (string-width relative) (string-width abbreviated))
        relative
      abbreviated)))

(defun ironic-rooster--split-command-line-1 (quoted-str)
  "Remove the escaped quotes and backlash from a QUOTED-STR.

Return a list of the final characters in the reverse order.

Only to be consumed by `ironic-rooster--split-command-line'."
  (let ((len (length quoted-str))
        (i 0)
        ch next-ch
        result)
    (while (< i len)
      (setq ch (aref quoted-str i))
      (when (eq ch ?\\)
        (let ((next-ch (and (< (1+ i) len)
                            (aref quoted-str (1+ i)))))
          (when (member next-ch '(?\\ ?\"))
            (setq ch next-ch)
            (cl-incf i))))
      (push ch result)
      (cl-incf i))
    result))

;; TODO: rewrite the function correctly to handle things like the following:
;;
;; "/usr/bin/clang++ -Irelative -DSOMEDEF=\"With spaces, quotes and \\-es.\" <args...>"
(defun ironic-rooster--split-command-line (cmd-line)
  "Split CMD-LINE into a list of arguments.

Takes care of double quotes as well as backslash.

Sadly I had to write this because `split-string-and-unquote'
breaks with escaped quotes in compile_commands.json, such as in:

    /usr/bin/c++ -DLLVM_VERSION_INFO=\\\\\\\"3.2svn\\\\\\\" <args>"
  ;; everytime I write a function like this one, it makes me feel bad
  (let* ((len (length cmd-line))
         (spaces (string-to-list " \f\t\n\r\v"))
         (first-not-spaces-re (concat "[^" spaces "]"))
         (i 0)
         ch
         args cur-arg)
    (while (< i len)
      (setq ch (aref cmd-line i))
      (cond
       ((member ch spaces)              ;spaces
        (when cur-arg
          (setq args (cons (apply 'string (nreverse cur-arg)) args)
                cur-arg nil))
        ;; move to the next char
        (setq i (or (string-match-p first-not-spaces-re cmd-line i)
                    len)))
       ((eq ch ?\")                     ;quoted string
        (let ((endq (string-match-p "[^\\]\"" cmd-line i)))
          (unless endq
            (error "Ironic-Rooster: ill formed command line"))
          (let ((quoted-str (substring cmd-line (1+ i) (1+ endq))))
            (setq cur-arg (append (ironic-rooster--split-command-line-1 quoted-str)
                                  cur-arg)
                  i (+ endq 2)))))
       (t                             ;a valid char
        ;; if it's an escape of: a backslash, a quote or a space push
        ;; only the following char.
        (when (eq ch ?\\)
          (let ((next-ch (and (< (1+ i) len)
                              (aref cmd-line (1+ i)))))
            (when (or (member next-ch '(?\\ ?\"))
                      (member next-ch spaces))
              (setq ch next-ch)
              (cl-incf i))))
        (push ch cur-arg)
        (cl-incf i))))
    (when cur-arg
      (setq args (cons (apply 'string (nreverse cur-arg)) args)))
    (nreverse args)))


;;
;; Mode
;;

(defvar ironic-rooster-mode-map (make-sparse-keymap)
  "Keymap used in `ironic-rooster-mode' buffers.")

;;;###autoload
(define-minor-mode ironic-rooster-mode
  "Minor mode for C, C++ and Objective-C, powered by libclang."
  nil
  ironic-rooster-lighter
  ironic-rooster-mode-map
  :group 'ironic-rooster
  (if ironic-rooster-mode
      (ironic-rooster--mode-enter)
    (ironic-rooster--mode-exit)))

(defun ironic-rooster--mode-enter ()
  ;; warn the user about modes such as php-mode who inherits c-mode
  (when (not (memq major-mode ironic-rooster-supported-major-modes))
    (display-warning 'ironic-rooster "Major mode is unknown to Ironic-Rooster,\
 see `ironic-rooster-supported-major-modes'."))
  ;; warn the user about Windows-specific issues
  (when (eq system-type 'windows-nt)
    (cond
     ((version< emacs-version "24.4")
      (display-warning 'ironic-rooster "Emacs >= 24.4 expected on Windows."))
     ((and (boundp 'w32-pipe-read-delay) (> w32-pipe-read-delay 0))
      (display-warning 'ironic-rooster "Performance will be bad because a\
 pipe delay is set for this platform (see variable\
 `w32-pipe-read-delay')."))))
  (ironic-rooster-completion--enter))

(defun ironic-rooster--mode-exit ()
  (ironic-rooster-completion--exit))

;;;###autoload
(defun ironic-rooster-version (&optional show-version)
  "Return the version number of the file ironic-rooster.el.

If called interactively display the version in the echo area."
  (interactive (list t))
  ;; Shamelessly stolen from `company-mode'.
  (with-temp-buffer
    (insert-file-contents (find-library-name "ironic-rooster"))
    (let ((v (lm-version)))
      (when show-version
        (message "ironic-rooster version: %s" v))
      v)))


;;
;; Compile options handling
;;

(defun ironic-rooster--lang-compile-option ()
  (ironic-rooster--awhen (cdr-safe (assq major-mode ironic-rooster-lang-compile-option-alist))
    (list "-x" it)))

(defun ironic-rooster--extract-working-directory-option (flags)
  "Return working directory specified on the command line, if
any."
  (catch 'found
    (while flags
      (let ((flag (car flags)))
        (cond
         ((string= "-working-directory" flag)
          (throw 'found (cadr flags)))
         ((string-prefix-p "-working-directory=" flag)
          (throw 'found (substring flag (length "-working-directory="))))
         (t
          (setq flags (cdr flags))))))))

(defun ironic-rooster--adjust-compile-options ()
  "The compile options to send to libclang."
  ;; TODO: if current buffer has no associated file (will be sent as '-') but is
  ;; in an existing directory, we will want to add -I (directory-file-name
  ;; buffer-file-name) to find the relative headers
  (append
   (ironic-rooster--lang-compile-option)
   (ironic-rooster--awhen ironic-rooster--working-directory
     (unless (ironic-rooster--extract-working-directory-option ironic-rooster--compile-options)
       (list "-working-directory" it)))
   ironic-rooster-additional-clang-options
   ironic-rooster--compile-options))

(defun ironic-rooster--extract-user-search-paths (compile-options work-dir)
  "Retrieve the user search paths present in COMPILE-OPTIONS.

Relative paths are expanded to be relative to WORK-DIR.

The returned paths are returned as
directory (`file-name-as-directory').

Note: WORK-DIR is not used when the compile option
'-working-directory=<directory>' is detected in COMPILE-OPTIONS."
  (setq work-dir (or (ironic-rooster--extract-working-directory-option compile-options)
                     work-dir))
  (let (include-dirs opt)
    (while (setq opt (car compile-options))
      (cond
       ((string= "-I" opt)
        (add-to-list 'include-dirs (nth 1 compile-options) t)
        (setq compile-options (cddr compile-options)))
       ((string-prefix-p "-I" opt)
        (add-to-list 'include-dirs (substring opt 2) t)
        (setq compile-options (cdr compile-options)))
       (t
        (setq compile-options (cdr compile-options)))))
    (delete-dups (mapcar #'(lambda (path)
                             (file-name-as-directory
                              (expand-file-name path work-dir)))
                         include-dirs))))


;;
;; Ironic-Rooster-Server setup
;;

(defvar ironic-rooster--server-install-command-history nil)
(defun ironic-rooster--install-server-read-command (command)
  (read-shell-command
   "Install command: " command
   (if (equal (car ironic-rooster--server-install-command-history) command)
       '(ironic-rooster--server-install-command-history . 1)
     'ironic-rooster--server-install-command-history)))

(defun ironic-rooster-install-server (command)
  "Install or reinstall the Ironic-Rooster server.

The installation requires CMake and the libclang developpement package."
  (interactive
   (list (let ((command
                (format
                 (concat "%s %s %s && %s --build . "
                         "--use-stderr --config Release --target install")
                 (shell-quote-argument ironic-rooster-cmake-executable)
                 (shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
                                               (expand-file-name
                                                ironic-rooster-server-install-prefix)))
                 (shell-quote-argument ironic-rooster-server-source-dir)
                 (shell-quote-argument ironic-rooster-cmake-executable))))
           (ironic-rooster--install-server-read-command command))))
  (let ((build-dir (or ironic-rooster-server-build-dir
                       (concat 
                        (file-name-as-directory temporary-file-directory)
                        (file-name-as-directory (format "build-ironic-rooster-server-%s"
                                                        (ironic-rooster-version)))))))
    (make-directory build-dir t)
    (let ((default-directory build-dir))
      ;; we need to kill the process to be able to install a new one,
      ;; at least on Windows
      (ironic-rooster-server-kill)
      (with-current-buffer (compilation-start command nil
                                              #'(lambda (maj-mode)
                                                  "*ironic-rooster-server build*"))
        (setq-local compilation-finish-functions
                    '(ironic-rooster--server-install-finish-function))))))

(defun ironic-rooster--server-install-finish-function (buffer msg)
  (if (string= "finished\n" msg)
      (message "ironic-rooster-server installed successfully!")
    (message "Failed to build ironic-rooster-server, you are on your own buddy!")))

(defun ironic-rooster--locate-server-executable ()
  "Check if an ironic-rooster-server exists for the current buffer."
  (let ((exe (expand-file-name "bin/ironic-rooster-server" ironic-rooster-server-install-prefix)))
    (condition-case err
        (let ((ironic-rooster-server-version (car (process-lines exe "--version"))))
          (if (and (string-match "^ironic-rooster-server version " ironic-rooster-server-version)
                   (version= (ironic-rooster-version)
                             (substring ironic-rooster-server-version
                                        (length "ironic-rooster-server version "))))
              ;; ironic-rooster-server is working and up-to-date!
              exe
            (message "ironic-rooster-server version mismatch: %s"
                     (substitute-command-keys
                      "type `\\[ironic-rooster-install-server]' to reinstall"))
            nil))
      (error
       (if (file-executable-p exe)
           ;; failed to execute due to a runtime problem, i.e: libclang.so isn't
           ;; in the ld paths
           (message "error: ironic-rooster-server is broken, good luck buddy! %s"
                    (error-message-string err))
         ;; ironic-rooster-server doesn't exists, first time ironic-rooster-mode is used? inform
         ;; the user about how to build the executable
         (message "%s"
                  (substitute-command-keys
                   "Type `\\[ironic-rooster-install-server]' to install ironic-rooster-server")))
       ;; return nil on error
       nil))))


;;
;; ironic-rooster-server process management.
;;

(defvar ironic-rooster--server-executable nil)
(defvar ironic-rooster--server-process nil)
(defvar ironic-rooster--server-buffer " *Ironic-Rooster*"
  "The name of the buffer for the ironic-rooster process to run in.

When using a leading space, the buffer is hidden from the buffer
list (and undo information is not kept).")

(defun ironic-rooster--start-server-process ()
  (when (setq ironic-rooster--server-executable (or ironic-rooster--server-executable
                                           (ironic-rooster--locate-server-executable)))
    (let ((process-connection-type nil)
          (process-adaptive-read-buffering nil)
          process)
      (setq process
            (start-process-shell-command
             "Ironic-Rooster"                    ;process name
             ironic-rooster--server-buffer       ;buffer
             (format "%s -i 2> %s"      ;command
                     (shell-quote-argument ironic-rooster--server-executable)
                     (expand-file-name
                      (format-time-string "ironic-rooster.%Y-%m-%d_%Hh-%Mm-%Ss.log")
                      temporary-file-directory))))
      (buffer-disable-undo ironic-rooster--server-buffer)
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel process 'ironic-rooster--server-process-sentinel)
      (set-process-filter process 'ironic-rooster--server-process-filter)
      process)))

;;;###autoload
(defun ironic-rooster-server-kill ()
  "Kill the running ironic-rooster-server process, if any."
  (interactive)
  (when (and ironic-rooster--server-process (process-live-p ironic-rooster--server-process))
    (kill-process ironic-rooster--server-process)
    (setq ironic-rooster--server-process nil)))

(defun ironic-rooster--get-server-process-create ()
  (if (and ironic-rooster--server-process
           (process-live-p ironic-rooster--server-process))
      ironic-rooster--server-process
    (setq ironic-rooster--server-process (ironic-rooster--start-server-process))))

(defun ironic-rooster--server-process-sentinel (process event)
  (unless (process-live-p process)
    (setq ironic-rooster--server-process nil)
    (message "ironic-rooster process stopped!")))

(defun ironic-rooster--process-server-response (process response)
  (let ((sexp (read response))
        (callback (ironic-rooster--server-process-pop-callback process)))
    (with-demoted-errors "Warning: %S"
      (apply (car callback) sexp (cdr callback)))))

(defun ironic-rooster--server-process-filter (process output)
  "Handle output that come from an ironic-rooster-server process."
  (let ((pbuf (process-buffer process))
        responses)
    ;; append output to process buffer
    (when (buffer-live-p pbuf)
      (with-current-buffer pbuf
        (save-excursion
          (goto-char (process-mark process))
          (insert output)
          (set-marker (process-mark process) (point))
          ;; check if the message is complete based on `ironic-rooster--eot'
          (goto-char (point-min))
          (while (search-forward ironic-rooster--eot nil t)
            (let ((response (buffer-substring-no-properties (point-min)
                                                            (point))))
              (delete-region (point-min) (point))
              (setq responses (cons response responses))))
          (goto-char (process-mark process)))))
    ;; Handle all responses.
    (mapc #'(lambda (r)
              (ironic-rooster--process-server-response process r))
          (nreverse responses))))

(defun ironic-rooster--server-process-push-callback (p cb)
  (let ((callbacks (process-get p 'ironic-rooster-callback-stack)))
    (if callbacks
        (nconc callbacks (list cb))
      (process-put p 'ironic-rooster-callback-stack (list cb)))))

(defun ironic-rooster--server-process-pop-callback (p)
  (let ((callbacks (process-get p 'ironic-rooster-callback-stack)))
    (process-put p 'ironic-rooster-callback-stack (cdr callbacks))
    (car callbacks)))


;;
;; Server commands
;;

(defun ironic-rooster--get-buffer-path-for-server ()
  "Get the path of the current buffer to send to ironic-rooster-server.

If no such file exists on the filesystem the special file '-' is
  returned instead."
  (if (and buffer-file-name (file-exists-p buffer-file-name))
      buffer-file-name
    "-"))

(defun ironic-rooster--send-request (request callback &rest args)
  (let ((process (ironic-rooster--get-server-process-create))
        (argv (cons request args)))
    (when (and process (process-live-p process))
      (ironic-rooster--server-process-push-callback process callback)
      ;; skip narrowing to compute buffer size and content
      (ironic-rooster--without-narrowing
        (process-send-string process
                             (format "%s\n"
                                     (combine-and-quote-strings argv)))))))

(defvar ironic-rooster--sync-id 0 "ID of next sync request.")
(defvar ironic-rooster--sync-result '(-1 . nil)
  "The car stores the id of the result and the cdr stores the return value.")

(defun ironic-rooster--sync-request-callback (response id)
  (setq ironic-rooster--sync-result (cons id response)))

(defun ironic-rooster--send-request-sync (request &rest args)
  "Send a request to ironic-rooster-server and wait for the result."
  (let* ((id ironic-rooster--sync-id)
         (callback (list #'ironic-rooster--sync-request-callback id)))
    (setq ironic-rooster--sync-id (1+ ironic-rooster--sync-id))
    (with-local-quit
      (let ((process (ironic-rooster--get-server-process-create)))
        (when process
          (apply 'ironic-rooster--send-request request callback args)
          (while (not (= id (car ironic-rooster--sync-result)))
            (accept-process-output process))
          (cdr ironic-rooster--sync-result))))))

(defun ironic-rooster--send-parse-request (request callback &rest args)
  "Send a request that acts on the current buffer to ironic-rooster-server.

This concerns mainly ironic-rooster-server commands that do some work on a
translation unit for libclang, the unsaved buffer data are taken
care of."
  (let ((process (ironic-rooster--get-server-process-create))
        (argv (append (list request
                            "--num-unsaved=1"
                            (ironic-rooster--get-buffer-path-for-server))
                      args))
        (compile-options (ironic-rooster--adjust-compile-options)))
    (when (and process (process-live-p process))
      (ironic-rooster--server-process-push-callback process callback)
      ;; skip narrowing to compute buffer size and content
      (ironic-rooster--without-narrowing
        ;; always make sure to finish with a newline (required by ironic-rooster-server
        ;; to play nice with line buffering even when the file doesn't end with
        ;; a newline)
        ;;
        ;; it is important to send the request atomically rather than using
        ;; multiple process-send calls. On Windows at least, if the request is
        ;; not atomic, content from subsequent requests can get intermixed with
        ;; earlier requests. This may be because of how Emacs behaves when the
        ;; buffers to communicate with processes are full (see
        ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Input-to-Processes.html).
        (process-send-string process
                             (format "%s\n%s\n%s\n%d\n%s\n"
                                     (combine-and-quote-strings argv)
                                     (combine-and-quote-strings compile-options)
                                     buffer-file-name
                                     (ironic-rooster--buffer-size-in-bytes)
                                     (buffer-substring (point-min) (point-max))))))))


;;
;; Buffer parsing
;;

(defvar-local ironic-rooster--parse-buffer-state nil
  "If non-nil, state is of the form (context . status) where:

- context is `ironic-rooster--parse-buffer-context'
- status is one of the following symbol: requested, done")

(defvar-local ironic-rooster--parse-buffer-callbacks nil)
(defvar-local ironic-rooster--parse-buffer-last-results nil
  "Holds the last parsing results.")

(defun ironic-rooster--parse-buffer-context ()
  ;; FIXME: use the ticks of all ironic-rooster's files that may influence this one?
  ;; FIXME: compile options should be part of the context? or settings the
  ;; compile options should flush the context alternatively.
  (buffer-chars-modified-tick))

(defun ironic-rooster--buffer-parsed-p (&optional ctx)
  (equal ironic-rooster--parse-buffer-state
         (cons (or ctx (ironic-rooster--parse-buffer-context)) 'done)))

(defun ironic-rooster--buffer-parsing-in-progress-p (&optional ctx)
  (equal ironic-rooster--parse-buffer-state
         (cons (or ctx (ironic-rooster--parse-buffer-context)) 'requested)))

(defun ironic-rooster--parse-request-handler (result context buffer)
  (with-current-buffer buffer
    (let ((callbacks ironic-rooster--parse-buffer-callbacks)
          (status (cond
                   ;; context out-of-date?
                   ((not (equal context (ironic-rooster--parse-buffer-context)))
                    'cancelled)
                   (result
                    'success)
                   (t
                    'failed))))
      (setq ironic-rooster--parse-buffer-last-results (list status)
            ironic-rooster--parse-buffer-callbacks nil
            ironic-rooster--parse-buffer-state (cons context 'done))
      (mapc #'(lambda (cb) (funcall cb status)) callbacks))))

;; TODO: provide a synchronous/blocking counterpart, see how
;; `url-retrieve-synchronously' does it
(defun ironic-rooster--parse-buffer-async (callback &optional force)
  "Parse the current buffer and call CALLACK when done.

Parsing is effectively done only if needed, if the buffer hasn't
changed since the last parsing, CALLBACK is called immediately.

Use FORCE to force a re-parse unconditionally.

Callback is a function that is called with one argument, the
status of the parsing request, the value is one of the following
symbol:

- success: parsing the file was a sucess, ironic-rooster-server has
  up-to-date information about the buffer

- failed: parsing the file resulted in a failure (file access
  rights wrong, whatever)

- cancelled: if the request for this callback was superseded by
  another request or if the callback is out-of-date (but not
  necessarily superseded by another request)"
  (when force
    (setq ironic-rooster--parse-buffer-state nil))
  (let ((context (ironic-rooster--parse-buffer-context)))
    (cond
     ((ironic-rooster--buffer-parsed-p context)
      ;; buffer already parsed, call callback immediately
      (apply callback ironic-rooster--parse-buffer-last-results))
     ((ironic-rooster--buffer-parsing-in-progress-p context)
      ;; the request is already pending, add callback to the list
      (push callback ironic-rooster--parse-buffer-callbacks))
     (t
      ;; current request is either out-of-date or inexistant,
      ;; cancel callbacks if any, and make new request
      (let ((obselete-callbacks ironic-rooster--parse-buffer-callbacks))
        (setq ironic-rooster--parse-buffer-callbacks (list callback)
              ironic-rooster--parse-buffer-state (cons context 'requested))
        (ironic-rooster--send-parse-request "parse"
                                   (list 'ironic-rooster--parse-request-handler context
                                         (current-buffer)))
        ;; it's safer to call this last, since the function may be called recursively
        (mapc #'(lambda (cb) (funcall cb 'cancelled)) obselete-callbacks))))))

(defun ironic-rooster-get-type--request-handler (types)
  (when types
    (if (cdr types)
        (if (string= (car types) (cadr types))
            (message "%s" (car types))
          (message "%s (aka '%s')" (car types) (cadr types)))
      (message "%s" (car types)))))

;;;###autoload
(defun ironic-rooster-get-type ()
    "Get the type of symbol under cursor."
  (interactive)
  (let ((line (line-number-at-pos))
        (column (1+ (- (position-bytes (point))
                       (position-bytes (point-at-bol))))))
    (ironic-rooster--parse-buffer-async
     (lambda (parse-status)
       (when (eq parse-status 'success)
         (ironic-rooster--send-request
          "get-type"
          (list 'ironic-rooster-get-type--request-handler)
          (number-to-string line)
          (number-to-string column)))))))

(provide 'ironic-rooster)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ironic-rooster.el ends here
