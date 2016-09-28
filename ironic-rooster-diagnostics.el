;;; ironic-rooster-diagnostics.el --- ironic-rooster-mode diagnostic reporting

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

;; Interface libclang's "Diagnostic Reporting", see
;; http://clang.llvm.org/doxygen/group__CINDEX__DIAG.html

;;; Code:

(require 'ironic-rooster)

(eval-when-compile
  (require 'cl))                        ;for lexical-let macro


;;
;; Ironic-Rooster Diagnostics Interface
;;

(defun ironic-rooster-diagnostics-file (diagnostic)
  (nth 0 diagnostic))

(defun ironic-rooster-diagnostics-line (diagnostic)
  (nth 1 diagnostic))

(defun ironic-rooster-diagnostics-column (diagnostic)
  (nth 2 diagnostic))

(defun ironic-rooster-diagnostics-severity (diagnostic)
  (nth 4 diagnostic))

(defun ironic-rooster-diagnostics-message (diagnostic)
  (nth 5 diagnostic))

(defun ironic-rooster-diagnostics--request-handler (diagnostics callback buffer)
  (with-current-buffer buffer
    (cond
     ((ironic-rooster--buffer-parsed-p)
      (funcall callback 'success diagnostics))
     (t
      ;; buffer has become out-of-date
      (funcall callback 'cancelled "diagnostics obselete, buffer has changed")))))

(defun ironic-rooster-diagnostics-async (callback &optional force)
  "Perform an asynchronous diagnostic request for the current
buffer.

Use FORCE to force the reparsing of the buffer.

CALLBACK is called with at least one argument, a symbol
representing the status of the request. Depending on the status
more argument are provided. Possible values are explained below:

- success

  When quering the diagnostics work, the additional argument is a
  list of diagnostic object, diagnostics fields can be queried
  with the functions `ironic-rooster-diagnostics-<xxx>'.

- error

  Retrieving the diagnostics wasn't possible. A string explaining
  the reason is passed as a second argument.

- cancelled

  Retrieving the diagnostics was cancelled, e.g: because the
  buffer has changed since the beginning of the request, and as
  such the diagnostics are considered no longer relevant. A
  reason string is passed as a second argument."
  (lexical-let ((cb callback))
    (ironic-rooster--parse-buffer-async
     #'(lambda (parse-status)
         (cond
          ((eq parse-status 'success)
           (ironic-rooster--send-request "diagnostics"
                                (list 'ironic-rooster-diagnostics--request-handler
                                      cb
                                      (current-buffer))))
          ((eq parse-status 'cancelled)
           (funcall cb 'cancelled "parsing was cancelled"))
          ((eq parse-status 'failed)
           (funcall cb 'error "parsing failed"))
          (t
           (funcall cb 'error "internal-error: unexpected parse status"))))
     force)))

(provide 'ironic-rooster-diagnostics)

;;; ironic-rooster-diagnostics.el ends here
