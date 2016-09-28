;;; ironic-rooster-cdb-json.el --- JSON Compilation Database support for ironic-rooster

;; Copyright (C) 2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords: c, convenience, tools

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
;; JSON Compilation Database support for Ironic-Rooster, see
;; http://clang.llvm.org/docs/JSONCompilationDatabase.html.
;;

;;; Code:

(require 'ironic-rooster-cdb)

(require 'cl-lib)

(require 'json)
(require 'pp)

(defvar ironic-rooster-cdb-json--project-alist nil
  "Alist of source directory and compile_commands.json locations.

Note, the compile_commands.json location may be relative to the
source directory.")

(defconst ironic-rooster-cdb-json--project-alist-file
  (concat ironic-rooster-user-dir "cdb-json-projects"))

;;;###autoload
(defun ironic-rooster-cdb-json (command &rest args)
  (cl-case command
    (get-compile-options (ironic-rooster-cdb-json--get-compile-options))))

;;;###autoload
(defun ironic-rooster-cdb-json-add-compile-commands-path (project-root
                                                 compile-commands-path)
  "Add an out-of-source compilation database.

Files below the PROJECT-ROOT directory will use the JSON
Compilation Database as specified by COMPILE-COMMANDS-PATH.

The JSON Compilation Database are often generated in the build
directory. This functions helps mapping out-of-source build
directories to project directory."
  (interactive
   (progn
     (let ((proot (read-directory-name "Project root:" nil nil t)))
       (list proot (read-file-name "Compile commands:" proot nil t
                                   "compile_commands.json")))))
  (add-to-list 'ironic-rooster-cdb-json--project-alist
               (cons (expand-file-name project-root)
                     (expand-file-name compile-commands-path)))
  (ironic-rooster-cdb-json--save-project-alist)

  ; and tell ironic-rooster to load it now
  (ironic-rooster-cdb-autosetup-compile-options))

(defun ironic-rooster-cdb-json--put-first (pos target-list)
  (if (>= pos (length target-list))
      target-list
    (let ((elm (nth pos target-list)))
      (append (list elm) (delete elm target-list)))))

(defun ironic-rooster-cdb-json--choose-cdb ()
  "Prompt to select CDB from current project root."
  (let* ((proot (ironic-rooster-cdb-json--find-best-prefix-path
                 (ironic-rooster-cdb-json--target-path)
                 (mapcar 'car ironic-rooster-cdb-json--project-alist)))
         (cdbs (mapcar 'cdr
                       (cl-remove-if-not (lambda (x) (string-equal proot (car x)))
                                         ironic-rooster-cdb-json--project-alist))))
    (completing-read "Choose Ironic-Rooster CDB: " cdbs nil 'require-match nil)))

;;;###autoload
(defun ironic-rooster-cdb-json-select ()
  "Select CDB to use with a prompt.

It is useful when you have several CDBs with the same project
root.

The completion function used internally is `completing-read' so
it could easily be used with other completion functions by
temporarily using a let-bind on `completing-read-function'. Or
even helm by enabling `helm-mode' before calling the function."
  (interactive)
  (let ((pos (cl-position (ironic-rooster-cdb-json--choose-cdb)
                          ironic-rooster-cdb-json--project-alist
                          :test (lambda (x y) (string-equal x (cdr y))))))
    (setq ironic-rooster-cdb-json--project-alist
          (ironic-rooster-cdb-json--put-first pos ironic-rooster-cdb-json--project-alist))
    (ironic-rooster-cdb-json--save-project-alist)
    (ironic-rooster-cdb-autosetup-compile-options)))

(defun ironic-rooster-cdb-json--last-mod (file)
  "File modification time or null time if file doesn't exist."
  (or (nth 5 (file-attributes file))
      '(0 0 0 0)))

;;;###autoload
(defun ironic-rooster-cdb-json-select-most-recent ()
  "Select CDB that is most recently modified."
  (interactive)
  (setq ironic-rooster-cdb-json--project-alist
        (sort ironic-rooster-cdb-json--project-alist
              (lambda (x y)
                (time-less-p (ironic-rooster-cdb-json--last-mod (cdr y))
                             (ironic-rooster-cdb-json--last-mod (cdr x))))))
  (ironic-rooster-cdb-json--save-project-alist)
  (ironic-rooster-cdb-autosetup-compile-options))

(defun ironic-rooster-cdb-json--get-compile-options ()
  (ironic-rooster--awhen (ironic-rooster-cdb-json--locate-db)
    (let ((db (ironic-rooster-cdb-json--load-db it)))
      (ironic-rooster--aif (ironic-rooster-cdb-json--exact-flags db)
          it
        (let ((dir-cdb (ironic-rooster-cdb-json--compute-directory-cdb db)))
          (ironic-rooster-cdb-json--guess-flags dir-cdb))))))

(defsubst ironic-rooster-cdb-json--target-path ()
  (or buffer-file-name (expand-file-name default-directory)))

(defun ironic-rooster-cdb-json--ensure-project-alist-loaded ()
  (unless ironic-rooster-cdb-json--project-alist
    (ironic-rooster-cdb-json--load-project-alist)))

(defun ironic-rooster-cdb-json--save-project-alist ()
  (with-temp-file ironic-rooster-cdb-json--project-alist-file
    (insert ";; -*- emacs-lisp -*-\n\
;;\n\
;; JSON Compilation Database project list.\n\
;;\n\
;; File auto-generated by ironic-rooster-cdb-json.\n\
;;\n")
    (pp ironic-rooster-cdb-json--project-alist (current-buffer))
    (insert "\n")))

(defun ironic-rooster-cdb-json--load-project-alist ()
  (when (file-exists-p ironic-rooster-cdb-json--project-alist-file)
    (setq ironic-rooster-cdb-json--project-alist
          (with-temp-buffer
            (insert-file-contents ironic-rooster-cdb-json--project-alist-file)
            (read (current-buffer))))))

(defun ironic-rooster-cdb-json--find-best-prefix-path (file prefixes)
  (cl-loop for prefix in prefixes
           with found = nil
           ;; keep the closest directory
           if (and (string-prefix-p prefix file)
                   (> (length prefix) (length found)))
           do (setq found prefix)
           finally return found))

(defun ironic-rooster-cdb-json--locate-db ()
  (ironic-rooster-cdb-json--ensure-project-alist-loaded)
  (ironic-rooster--aif (ironic-rooster-cdb-json--find-best-prefix-path
               (ironic-rooster-cdb-json--target-path)
               (mapcar 'car ironic-rooster-cdb-json--project-alist))
      (expand-file-name
       (cdr (assoc it ironic-rooster-cdb-json--project-alist))
       it)
    ;; If not in the project table, look in the dominating directories
    (ironic-rooster--awhen (ironic-rooster-cdb--locate-dominating-file-with-dirs
                   (ironic-rooster-cdb-json--target-path)
                   "compile_commands.json"
                   ironic-rooster-cdb-search-directory-list)
      (expand-file-name it))))

(defun ironic-rooster-cdb-json--load-db (json-file)
  (delq nil (mapcar #'ironic-rooster-cdb-json--transform-compile-command
                    ;; JSON read may throw
                    (json-read-file json-file))))

(defun ironic-rooster-cdb-json--exact-flags (file-cdb)
  (when buffer-file-name
    (mapcar #'(lambda (e)
                (cons (nth 1 e) (nth 2 e)))
            (ironic-rooster--assoc-all buffer-file-name file-cdb))))

(defun ironic-rooster-cdb-json--guess-flags (dir-cdb)
  (cl-loop for e in dir-cdb
           with buf-path = (ironic-rooster-cdb-json--target-path)
           with found = nil
           for dir = (car e)
           ;; keep the closest directory
           if (and (string-prefix-p dir buf-path)
                   (> (length dir) (length found)))
           do (setq found e)
           finally return (list (cons (nth 1 found) (nth 2 found)))))

(defsubst ironic-rooster-cdb-json--compile-command-directory (compile-command)
  (cdr (assq 'directory compile-command)))

(defsubst ironic-rooster-cdb-json--compile-command-file (compile-command)
  (cdr (assq 'file compile-command)))

(defun ironic-rooster-cdb-json--compile-command-options (compile-command)
  "Return the compile options of COMPILE-COMMAND as a list."
  (ironic-rooster-cdb--remove-compiler-from-flags
   (ironic-rooster--split-command-line (cdr (assq 'command compile-command)))))

(defun ironic-rooster-cdb-json--adjust-compile-options (compile-options file default-dir)
  "Adjust COMPILE-OPTIONS to only use options useful for parsing.

COMPILE-OPTIONS is modified by side effects but the returned list
should be used since elements can change at the head.

Removes the input file, the output file, ...

Relative paths are relative to DEFAULT-DIR."
  ;; compute the absolute path for FILE only once
  (setq file (expand-file-name file default-dir))
  (let* ((head (cons 'nah compile-options))
         (it head)
         opt)
    (while (setq opt (cadr it))
      (cond
       ;; end of options, skip all positional arguments (source files)
       ((string= opt "--")
        (setcdr it nil))
       ;; strip -c
       ((string= "-c" opt)
        (setcdr it (nthcdr 2 it)))
       ;; strip -o <output-file> and -o<output-file>
       ((string-prefix-p "-o" opt)
        (if (string= opt "-o")
            (setcdr it (nthcdr 3 it))
          (setcdr it (nthcdr 2 it))))
       ;; skip input file
       ((string= file (expand-file-name opt default-dir))
        (setcdr it (nthcdr 2 it)))
       (t
        ;; if head of cdr hasn't been skipped, iterate, otherwise check if the
        ;; new cdr need skipping
        (setq it (cdr it)))))
    (cdr head)))

(defun ironic-rooster-cdb-json--transform-compile-command (compile-command)
  "Transform a compile command in the JSON compilation database
into a friendlier format.

The returned value is a list composed of the following elements:
0. The absolute path to the file.
1. The compile options.
2. The invocation directory. Relative paths in the compile
   options elements are relative to this directory.

Return nil if the compile command is invalid or the compile
options are empty."
  (let* ((directory (ironic-rooster-cdb-json--compile-command-directory compile-command))
         (path (expand-file-name
                (ironic-rooster-cdb-json--compile-command-file compile-command) directory))
         (options (ironic-rooster-cdb-json--compile-command-options compile-command)))
    (when (and path directory options)
      (list path
            (ironic-rooster-cdb-json--adjust-compile-options options path directory)
            directory))))

(defun ironic-rooster-cdb-json--compute-directory-cdb (file-cdb)
  ;; collect flags by directory, e.g: for headers in source directories or
  ;; new files that are not yet present in the compilation database
  (let ((dir-cdb (ironic-rooster-cdb-json--collect-compile-options-by-dir file-cdb)))
    (nconc dir-cdb
           ;; collect flags for header search paths too
           (ironic-rooster-cdb-json--collect-compile-options-for-include-dirs dir-cdb))))

(defun ironic-rooster-cdb-json--collect-compile-options-by-dir (file-cdb)
  "Collect the compile options per directory from a file compilation database.

The returned value similar to
`ironic-rooster-cdb-json--transform-compile-command' except for the first
argument which represents a whole directory (ending with slash on
Unix, `file-name-as-directory') instead of a single file."
  (let ((dir-cdb (delete-dups
                  (mapcar #'(lambda (e)
                              (cons (file-name-directory (car e)) (cdr e)))
                          file-cdb))))
    ;; TODO: remove directories when a parent directory has the same flags, for
    ;; example, writing the following in CMake:
    ;;     add_executable(exe foo.cpp sub/bar.cpp)
    ;; will result in duplicated compile options for the subdirectory 'sub/'.
    dir-cdb))

(defun ironic-rooster-cdb-json--collect-compile-options-for-include-dirs (dir-cdb)
  "Guess the compile options to use for directories in the search path.

The returned value is in the same format as the input value, see
`ironic-rooster-cdb-json--collect-compile-options-for-include-dirs'."
  (let ((include-dirs (delete-dups (mapcar 'car dir-cdb)))
        out)
    (dolist (e dir-cdb)
      (dolist (dir (ironic-rooster--extract-user-search-paths (nth 1 e) (nth 2 e)))
        (unless (member dir include-dirs)
          (setq include-dirs (cons dir include-dirs)
                out (cons (cons dir (cdr e)) out)))))
    out))

(provide 'ironic-rooster-cdb-json)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ironic-rooster-cdb-json ends here
