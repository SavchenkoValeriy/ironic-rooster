;;; ironic-rooster-completion.el --- ironic-rooster-mode completion interface

;; Copyright (C) 2012-2014  Guillaume Papin

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

;; Handle the search of completion points, the triggering of the
;; completion when needed and the "parsing" of completion results.

;;; Code:

(require 'ironic-rooster)
(require 'ironic-rooster-snippet)

(require 'cl-lib)


;;
;; Customizable variables
;;

(defgroup ironic-rooster-completion nil
  "Ironic-Rooster's completion interface."
  :group 'ironic-rooster)

(defcustom ironic-rooster-completion-trigger-commands '(self-insert-command
                                               newline-and-indent
                                               c-context-line-break
                                               c-scope-operator
                                               ;; electric commands
                                               c-electric-backspace
                                               c-electric-brace
                                               c-electric-colon
                                               c-electric-lt-gt
                                               c-electric-paren
                                               c-electric-pound
                                               c-electric-semi&comma
                                               c-electric-slash
                                               c-electric-star)
  "List of commands to watch for asynchronous completion triggering."
  :type '(repeat function)
  :group 'ironic-rooster-completion)


;;
;; Internal variables
;;

(defvar-local ironic-rooster-completion--context nil)
(defvar-local ironic-rooster-completion--context-tick 0)
(defvar-local ironic-rooster-completion--request-callbacks nil)
(defvar-local ironic-rooster-completion--request-tick 0)
(defvar-local ironic-rooster-completion--candidates nil)
(defvar-local ironic-rooster-completion--candidates-tick 0)


;;
;; Utility functions
;;

(defun ironic-rooster-completion-symbol-bounds ()
  (let ((pt (point))
        (syntax (syntax-ppss)))
    ;; no prefix for strings or comments
    ;; TODO: Use fontlock faces instead? at least
    ;;     #warning In the middle of a warning|
    ;; will be handled properly but things like links when
    ;; `goto-address-prog-mode' is enabled will mess up things:
    ;;     #error see bug report XY: http://example.com/XY
    (unless (or (nth 3 syntax)          ;skip strings
                (nth 4 syntax))         ;skip comments
      (save-excursion
        (skip-chars-backward "_a-zA-Z0-9")
        (let ((ch (char-after)))
          (unless (and ch (>= ch ?0) (<= ch ?9)) ;skip numbers
            (when (eq (char-before) ?~)
              (backward-char))
            (setq pt (point))
            (skip-chars-forward "_a-zA-Z0-9~")
            (cons pt (point))))))))

(defun ironic-rooster-completion-beginning-of-symbol ()
  (car (ironic-rooster-completion-symbol-bounds)))

(defun ironic-rooster-completion-end-of-symbol ()
  (cdr (ironic-rooster-completion-symbol-bounds)))

(defsubst ironic-rooster-completion--skip-whitespace-backwards ()
  ;;(skip-syntax-backward "-") doesn't seem to care about newlines
  (skip-chars-backward " \t\n\r"))

(defun ironic-rooster-completion--context-pos ()
  (ironic-rooster--awhen (ironic-rooster-completion-beginning-of-symbol)
    (save-excursion
      (goto-char it)
      (ironic-rooster-completion--skip-whitespace-backwards)
      (point))))


;;
;; Functions
;;

(defun ironic-rooster-completion--enter ()
  (add-hook 'post-command-hook 'ironic-rooster-completion--post-command nil t)
  (add-hook 'completion-at-point-functions 'ironic-rooster-completion-at-point nil t))

(defun ironic-rooster-completion--exit ()
  (remove-hook 'post-command-hook 'ironic-rooster-completion--post-command t)
  (remove-hook 'completion-at-point-functions 'ironic-rooster-completion-at-point t)
  (setq ironic-rooster-completion--context nil
        ironic-rooster-completion--candidates nil
        ironic-rooster-completion--context-tick 0
        ironic-rooster-completion--request-tick 0
        ironic-rooster-completion--request-callbacks nil
        ironic-rooster-completion--candidates-tick 0))

(defun ironic-rooster-completion--post-command ()
  (when (and (memq this-command ironic-rooster-completion-trigger-commands)
             (ironic-rooster-completion--update-context)
             (ironic-rooster-completion-at-trigger-point-p))
    (ironic-rooster-completion--send-request)))

(defun ironic-rooster-completion--update-context ()
  "Update the completion context variables based on the current position.

Return t if the context has been updated, nil otherwise."
  (let ((ctx (ironic-rooster-completion--context-pos)))
    (if (eq ctx ironic-rooster-completion--context)
        nil
      (setq ironic-rooster-completion--context ctx
            ironic-rooster-completion--candidates nil
            ironic-rooster-completion--context-tick (1+ ironic-rooster-completion--context-tick))
      (unless ironic-rooster-completion--context
        ;; when there is no context, assume that the candidates are available
        ;; even though they are nil
        ironic-rooster-completion--request-tick ironic-rooster-completion--context-tick
        ironic-rooster-completion--request-callbacks nil
        ironic-rooster-completion--candidates nil
        ironic-rooster-completion--candidates-tick ironic-rooster-completion--context-tick)
      t)))

(defun ironic-rooster-completion--post-complete-yas-snippet (str placeholders)
  (let ((ph-count 0)
        (from 0)
        to snippet)
    (while
        (setq to (car placeholders)
              snippet (concat
                       snippet
                       (substring str from to)
                       (format "${%d:%s}"
                               (cl-incf ph-count)
                               (substring str
                                          (car placeholders)
                                          (cadr placeholders))))
              from (cadr placeholders)
              placeholders (cddr placeholders)))
    ;; handle the remaining non-snippet string, if any.
    (concat snippet (substring str from) "$0")))


;;
;; Interface with ironic-rooster-server
;;

(defun ironic-rooster-completion--send-request ()
  (let (line column)
    (save-excursion
      (goto-char (ironic-rooster-completion-beginning-of-symbol))
      ;; `position-bytes' to handle multibytes and 'multicolumns' (i.e
      ;; tabulations) characters properly
      (ironic-rooster--without-narrowing
        (setq line (line-number-at-pos)
              column (1+ (- (position-bytes (point))
                            (position-bytes (point-at-bol)))))))
    (setq ironic-rooster-completion--request-callbacks nil
          ironic-rooster-completion--request-tick ironic-rooster-completion--context-tick)
    (ironic-rooster--send-parse-request
     "complete"
     (list 'ironic-rooster-completion--request-handler ironic-rooster-completion--context-tick)
     (number-to-string line)
     (number-to-string column))))

(defun ironic-rooster-completion--request-handler (candidates tick)
  (when (eq tick ironic-rooster-completion--context-tick)
    (setq
     ironic-rooster-completion--candidates-tick tick
     ironic-rooster-completion--candidates candidates)
    (mapc 'funcall ironic-rooster-completion--request-callbacks)))

(defun ironic-rooster-completion--still-completing-p ()
  (unless (ironic-rooster-completion-candidates-available-p)
    (eq ironic-rooster-completion--request-tick ironic-rooster-completion--context-tick)))


;;
;; Ironic-Rooster Completion Interface
;;

(defun ironic-rooster-completion-typed-text (candidate)
  (nth 0 candidate))

(defun ironic-rooster-completion-priority (candidate)
  (nth 1 candidate))

(defun ironic-rooster-completion-type (candidate)
  (nth 2 candidate))

(defun ironic-rooster-completion-brief (candidate)
  (nth 3 candidate))

(defun ironic-rooster-completion-annotation (candidate)
  (substring (nth 4 candidate) (nth 5 candidate)))

(defun ironic-rooster-completion-post-comp-str (candidate)
  (car (nth 6 candidate)))

(defun ironic-rooster-completion-post-comp-placeholders (candidate)
  (cdr (nth 6 candidate)))

(defun ironic-rooster-completion-candidates-available-p ()
  (and (eq (ironic-rooster-completion--context-pos) ironic-rooster-completion--context)
       (eq ironic-rooster-completion--candidates-tick ironic-rooster-completion--context-tick)))

(defun ironic-rooster-completion-candidates ()
  "Return the list of candidates at point, if available.

Use the function `ironic-rooster-completion-candidates-available-p' to
know if the candidate list is available.

A candidate is composed of the following elements:
 0. The typed text. Multiple candidates can share the same string
    because of overloaded functions, default arguments, etc.
 1. The priority.
 2. The [result-]type of the candidate, if any.
 3. If non-nil, contains the Doxygen brief documentation of the
    candidate.
 4. The signature of the candidate excluding the result-type
    which is available separately.
    Example: \"foo(int a, int b) const\"
 5. The annotation start, a 0-based index in the prototype string.
 6. Post-completion data. The text to insert followed by 0 or
    more indices. These indices work by pairs and describe ranges
    of placeholder text.
    Example: (\"(int a, int b)\" 1 6 8 13)"
  (and (ironic-rooster-completion-candidates-available-p)
       ironic-rooster-completion--candidates))

(defun ironic-rooster-completion-candidates-async (callback)
  "Call CALLBACK when asynchronous completion is available.

Note that:
 - If the candidates are already available, CALLBACK is called
   immediately.
 - In some circumstances, CALLBACK may not be called. i.e:
   ironic-rooster-server crashes, ..."
  (ironic-rooster-completion--update-context)
  (if (ironic-rooster-completion-candidates-available-p)
      (funcall callback)
    (when ironic-rooster-completion--context
      (unless (ironic-rooster-completion--still-completing-p)
        (ironic-rooster-completion--send-request))
      (setq ironic-rooster-completion--request-callbacks
            (cons callback ironic-rooster-completion--request-callbacks)))))

(defun ironic-rooster-completion-post-complete (candidate)
  (let ((str (ironic-rooster-completion-post-comp-str candidate))
        (placeholders (ironic-rooster-completion-post-comp-placeholders candidate)))
    (if (and placeholders (ironic-rooster-snippet-available-p))
        (ironic-rooster-snippet-expand
         (ironic-rooster-completion--post-complete-yas-snippet str placeholders))
      (insert (substring str 0 (car placeholders))))))

(defun ironic-rooster-completion-at-trigger-point-p ()
  (when (eq (point) (ironic-rooster-completion-beginning-of-symbol))
    (save-excursion
      (cond
       ;; use `re-search-backward' so that the cursor is moved just before the
       ;; member access, if any
       ((re-search-backward
         (format "%s\\=" (regexp-opt '("."     ;object member access
                                       "->"    ;pointer member access
                                       "::"))) ;scope operator
         (point-at-bol) t)
        (unless
            ;; ignore most common uses of '.' where it's not a member access
            (and (eq (char-after) ?.)
                 (or
                  ;; include statements: #include <foo.|>
                  (looking-back  "^#\\s-*include\\s-+[<\"][^>\"]*"
                                 (point-at-bol))
                  ;; floating point numbers (not thorough, see:
                  ;; http://en.cppreference.com/w/cpp/language/floating_literal)
                  (looking-back "[^_a-zA-Z0-9][[:digit:]]+" (point-at-bol))))
          ;; except the above exceptions we use a "whitelist" for the places
          ;; where it looks like a member access
          (ironic-rooster-completion--skip-whitespace-backwards)
          (or
           ;; after brackets consider it's a member access so things like
           ;; 'getFoo().|' match
           (memq (char-before) (list ?\) ?\] ?} ?>))
           ;; identifiers but ignoring some keywords
           ;;
           ;; handle only a subset of template parameter packs, where the
           ;; ellipsis is preceded by a keyword, in situation like:
           ;;     template<typename ... Args> class X {...};
           ;;     template<typename .|
           ;; or just look if the face is: font-lock-keyword-face?
           (save-excursion
             (and (re-search-backward "\\b\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\="
                                      (point-at-bol) t)
                  (not (member (match-string 0)
                               '("class" "sizeof" "typename"))))))))))))


;;
;; Ironic-Rooster CAPF
;;

(defun ironic-rooster-completion--at-point-annotate (candidate)
  (ironic-rooster-completion-annotation
   (get-text-property 0 'ironic-rooster-capf candidate)))

;;;###autoload
(defun ironic-rooster-completion-at-point ()
  (when (and ironic-rooster-mode (ironic-rooster-completion-candidates-available-p))
    (let ((symbol-bounds (ironic-rooster-completion-symbol-bounds)))
      (list
       (car symbol-bounds)              ;start
       (cdr symbol-bounds)              ;end
       (mapcar #'(lambda (candidate)    ;completion table
                   (propertize (car candidate) 'ironic-rooster-capf candidate))
               (ironic-rooster-completion-candidates))
       :annotation-function 'ironic-rooster-completion--at-point-annotate))))

;;;###autoload
(defun ironic-rooster-completion-at-point-async ()
  (interactive)
  (ironic-rooster-completion-candidates-async 'completion-at-point))

(provide 'ironic-rooster-completion)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ironic-rooster-completion.el ends here
