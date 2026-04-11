;;; hidepass.el --- Hide passwords at one or multiple lines  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: hide, hidden, password, faces
;; URL: https://codeberg.org/Anoncheg/emacs-hidepass
;; Version: 0.1
;; Created: 11 apr 2026
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>

;;; Commentary:

;;; Configuration:

;; (add-to-list 'load-path "path/to/hidepass") ; (optional)
;; (require 'hidepass)

;;; Usage:
;; M-x hidepass-mode
;; or add to a file with passwords atthe  first line :
;; ; -*- eval: (hidepass-mode 1) -*-

;;; Code:

;; Touch: It is a gasket problem, probably.

(defgroup hidepass nil
  "Settings for hiding passwords."
  :group 'hidepass)

(defcustom hidepass-patterns '("[pP]ass:? \\(.+\\)$"
                               "[lL]ogin:? \\(.+\\)$"
                               "[tT]oken:? \\(.+\\)$")
  "Regex patterns with one capturing group."
  :type '(repeat regexp)
  :group 'hidepass)

(defcustom hidepass-multiline-patterns nil
  ;; '(("^#\\+begin_src pass\n" . "\n#\\+end_src")
  ;;   ("<secret>" . "</secret>")
  ;;   ("---BEGIN PASSWORD---" . "---END PASSWORD---"))
  "List of (START-REGEXP . END-REGEXP) for multiline masking.
The text between these two regexps will be hidden.
Case are ignored."
  :type '(repeat (cons regexp regexp))
  :group 'hidepass)

(defcustom hidepass-mask "******"
  "String to obscure passwords with."
  :type 'string
  :group 'hidepass)

(defcustom hidepass-hide-first-line nil
  "Hide the first line of the buffer."
  :type 'boolean
  :group 'hidepass)

(defun hidepass-render ()
  "Render a password (hidden) using a display property."
  `(face font-lock-doc-face display ,hidepass-mask))

(defun hidepass-render-multiline ()
  "Render a multiline password, preserving the line structure."
  (let* ((beg (match-beginning 1))
         (end (match-end 1))
         (content (buffer-substring-no-properties beg end))
         ;; Use * instead of + to ensure empty lines/spaces are also masked
         (mask (replace-regexp-in-string "[^\n]*" hidepass-mask content)))
    `(face font-lock-doc-face display ,mask)))


(defun hidepass--match-multiline-block (limit)
  "Reliable multiline matcher that supports line-by-line selection."
  (let ((case-fold-search t)
        (start-pt (point))
        best-match)
    ;; 1. FIND THE BEST (EARLIEST) MATCH
    (dolist (pair hidepass-multiline-patterns)
      (save-excursion
        (goto-char start-pt)
        (when (re-search-forward (car pair) limit t)
          (let ((m-start (match-beginning 0))
                (c-start (match-end 0))
                (end-re (cdr pair)))
            (when (re-search-forward end-re nil t)
              (let ((m-end (match-end 0))
                    (c-end (match-beginning 0)))
                (when (or (not best-match) (< m-start (car best-match)))
                  (setq best-match (list m-start m-end c-start c-end)))))))))

    ;; 2. IF FOUND, APPLY LINE-BY-LINE MASKING AND UPDATE FONT-LOCK
    (when best-match
      (let ((c-start (nth 2 best-match))
            (c-end (nth 3 best-match)))
        ;; Apply properties manually to each line for selectability
        (save-excursion
          (goto-char c-start)
          (while (< (point) c-end)
            (let ((line-end (min (line-end-position) c-end)))
              (when (< (point) line-end)
                (put-text-property (point) line-end 'display hidepass-mask)
                (put-text-property (point) line-end 'face 'font-lock-doc-face))
              (forward-line 1)
              (if (eobp) (goto-char c-end)))))

        ;; Set match data and move point past the whole block
        (set-match-data best-match)
        (goto-char (nth 1 best-match))
        t))))


(defun hidepass-font-lock-keywords ()
  "Define the patterns to mask via font-lock."
  `(;; 1. Multiline patterns (Self-rendering)
    (hidepass--match-multiline-block 0 nil)
    ;; 2. The first line pattern (single line)
    ,@(when hidepass-hide-first-line
        '(("\\`\\(.*\\)$" 1 (hidepass-render))))
    ;; 3. Standard regex patterns (single line)
    ,@(mapcar (lambda (pat) `(,pat 1 (hidepass-render)))
              hidepass-patterns)))


(defun hidepass-on ()
  "Font-lock configuration and refresh."
  (let ((props (make-local-variable 'font-lock-extra-managed-props)))
    (add-to-list props 'display))
  (when hidepass-multiline-patterns
    (setq-local font-lock-multiline t))
  (font-lock-add-keywords nil (hidepass-font-lock-keywords) 'prepend) ; 'prepend required for Org mode
  ;; (font-lock-add-keywords nil (hidepass-font-lock-keywords))
  (font-lock-flush))


(defun hidepass-off ()
  "Remove keywords from font-locks and refresh."
  (font-lock-remove-keywords nil (hidepass-font-lock-keywords))
  (font-lock-flush))


;;;###autoload
(define-minor-mode hidepass-mode
  "Minor mode for hiding password in any text mode, including Org mode."
  :lighter " Hidepass"
  (if hidepass-mode
      (hidepass-on)
    (hidepass-off))
  (font-lock-mode 1))

(provide 'hidepass)
;;; hidepass.el ends here
