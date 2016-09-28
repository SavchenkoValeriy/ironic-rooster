;;; ironic-rooster-snippet.el --- Snippet support for Ironic-Rooster-Mode

;; Copyright (C) 2013-2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords: c, convenience

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

;; Snippet support functions, support all version of YASnippet and
;; maybe further frameworks later.
;;
;; It is possible to check snippet support by calling
;; `ironic-rooster-snippet-available-p'.
;;
;; Note: Some YASnippet versions require to have
;;       `yas/minor-mode' or `yas-minor-mode' enabled to work.

;;; Code:


;; Private variables
;;
(defvar ironic-rooster-snippet--expand-function nil
  "Function to expand a snippet at a given point (by default to
the current position).

e.g: (defun my-expand-snippet (snippet-str &optional pos)
       (do-stuff (or pos (point)))

Will be set to nil if no snippet expansion function is found.")


;;
;; "Public" functions
;;
(defun ironic-rooster-snippet-available-p ()
  "Return t if snippets are supported."
  (and (ironic-rooster-snippet--get-expand-function)
       (not (ironic-rooster-snippet--yas-disabled-p))))

(defun ironic-rooster-snippet-expand (snippet-str &optional pos)
  "Expand SNIPPET-STR starting at POS.

If `ironic-rooster-snippet-available-p' return t then"
  (let ((expand-func (ironic-rooster-snippet--get-expand-function)))
    (funcall expand-func snippet-str pos)))


;; Private functions
;;
(defun ironic-rooster-snippet--get-expand-function ()
  (unless ironic-rooster-snippet--expand-function
    (ironic-rooster-snippet--init-yas))
  ironic-rooster-snippet--expand-function)

(defun ironic-rooster-snippet--init-yas ()
  ;; find the snippet expand function
  (when (require 'yasnippet nil t)
    (let ((yas-version (or (and (boundp 'yas--version) yas--version)
                           (and (boundp 'yas/version) yas/version)))) ;for old versions
      (when (stringp yas-version)
        (setq yas-version (replace-regexp-in-string "(\\|)" "" yas-version))
        (setq ironic-rooster-snippet--expand-function
              ;; (string= ... "0.6.0c"), the 'c' suffix is not supported by
              ;; `version-to-list' in emacs versions < 24, treat this one
              ;; specifically.
              (cond
               ((or (string= yas-version "0.6.0c")
                    (version<= yas-version "0.6.0b"))
                'ironic-rooster-snippet--expand-yas-1)
               ;; `version<' thinks "0.8beta" < "0.8", we want to consider
               ;; anything starting with "0.8" as "0.8" and more.
               ((and (version< yas-version "0.8")
                     (not (string-prefix-p "0.8" yas-version)))
                'ironic-rooster-snippet--expand-yas-2)
               (t
                'ironic-rooster-snippet--expand-yas-3)))))))

(defun ironic-rooster-snippet--yas-disabled-p ()
  "If the current yasnippet version offers a minor-mode, check if
this mode is disable by returning t, otherwise returns nil and
it's partially safe to assume that yasnippet expansion can be
used."
  ;; XXX: work only when yasnippet is enabled, otherwise some
  ;;      variables used for the snippet expansion are not set and it
  ;;      causes some errors.
  (if (boundp 'yas-minor-mode)
      (not yas-minor-mode)
    (if (boundp 'yas/minor-mode)
        (not yas/minor-mode))))

(defun ironic-rooster-snippet--expand-yas-1 (snippet-str &optional pos)
  "Expand snippets for YASnippet version <= 0.6.0c."
  (declare-function yas/expand-snippet "ext:yasnippet" t nil)
  (unless (ironic-rooster-snippet--yas-disabled-p)
    (yas/expand-snippet (or pos (point))
                        (or pos (point))
                        snippet-str)))

(defun ironic-rooster-snippet--expand-yas-2 (snippet-str &optional pos)
  "Expand snippets for YASnippet version < 0.8.

See also `ironic-rooster-snippet--expand-yas-1'."
  (declare-function yas/expand-snippet "ext:yasnippet" t nil)
  (unless (ironic-rooster-snippet--yas-disabled-p)
    (when pos
      (goto-char pos))
    (yas/expand-snippet snippet-str)))

(defun ironic-rooster-snippet--expand-yas-3 (snippet-str &optional pos)
  "Expand snippets for YASnippet version >= 0.8.

See also `ironic-rooster-snippet--expand-yas-2'."
  (declare-function yas-expand-snippet "ext:yasnippet" t nil)
  (unless (ironic-rooster-snippet--yas-disabled-p)
    (when pos
      (goto-char pos))
    (yas-expand-snippet snippet-str)))

(provide 'ironic-rooster-snippet)
;;; ironic-rooster-snippet.el ends here
