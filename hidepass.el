;;; hidepass.el --- Hide passwords at one or multiple lines  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: hide, hidden, password, faces
;; URL: https://codeberg.org/Anoncheg/emacs-hidepass
;; Version: 0.1
;; Created: 11 apr 2026
;; Package-Requires: ((emacs "27.2"))
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

;; Configuration:

;; (add-to-list 'load-path "path/to/hidepass") ; (optional)
;; (require 'hidepass)

;; Multiline password is disabled by default, configure it with:
;; (setopt hidepass-multiline-patterns
;;   '(("^#\\+begin_src pass\n" . "^#\\+end_src")
;;     ("<secret>" . "</secret>")
;;     ("---BEGIN PASSWORD---" . "---END PASSWORD---")))

;;; Usage:

;; M-x hidepass-mode
;; or add to a file with passwords atthe  first line :
;; ; -*- eval: (hidepass-mode 1) -*-

;;; Customization:

;; M-x customize-group RET hidepass RET

;;; Other packages
;; - Navigation in Dired, Packages, Buffers modes https://github.com/Anoncheg1/firstly-search
;; - Search with Chinese	https://github.com/Anoncheg1/pinyin-isearch
;; - Ediff fix		https://github.com/Anoncheg1/ediffnw
;; - Dired history	https://github.com/Anoncheg1/dired-hist
;; - Selected window contrast	https://github.com/Anoncheg1/selected-window-contrast
;; - Copy link to clipboard	https://github.com/Anoncheg1/emacs-org-links
;; - Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
;; - Restore buffer state		https://github.com/Anoncheg1/emacs-unmodified-buffer1
;; - outline.el usage		https://github.com/Anoncheg1/emacs-outline-it
;; - Call LLMs and AI agents from Org-mode ai block. https://github.com/Anoncheg1/emacs-oai

;;; Donate, sponsor author
;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether TRX-TRON) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
;; - TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D

;;; Code:

;; Touch: revolution as a luck of evolution.

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
  ;; '(("^#\\+begin_src pass\n" . "^#\\+end_src")
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


(defun hidepass--match-multiline-block (limit)
  "Reliable multiline matcher that supports line-by-line selection.
Argument LIMIT bounds the search is font-lock specific."
  (let ((case-fold-search t)
        (start-pt (point))
        best-match)
    ;; 1. Find the match
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
    ;; 2. Apply and Return
    (when best-match
      (let* ((m-start (max (point-min) (nth 0 best-match)))
             (m-end   (nth 1 best-match))
             (c-start (nth 2 best-match))
             (c-end   (nth 3 best-match)))

        (save-excursion
          (goto-char c-start)
          (while (< (point) c-end)
            (let ((line-end (min (line-end-position) c-end)))
              (when (< (point) line-end)
                (font-lock-prepend-text-property (point) line-end 'display hidepass-mask)
                (font-lock-prepend-text-property (point) line-end 'face 'font-lock-doc-face))
              (forward-line 1)
              (if (eobp) (goto-char c-end)))))
        ;; We provide ONLY the start and end of the whole match (Group 0).
        (set-match-data (list m-start m-end))
        (goto-char m-end)
        t))))


(defun hidepass-font-lock-keywords ()
  "Define the patterns to mask via font-lock."
  `(;; 1. Multiline patterns (Self-rendering)
    (hidepass--match-multiline-block (0 nil))
    ;; 2. The first line pattern (single line)
    ,@(when hidepass-hide-first-line
        '(("\\`\\(.*\\)$" 1 (hidepass-render))))
    ;; 3. Standard regex patterns (single line)
    ,@(mapcar (lambda (pat) `(,pat 1 (hidepass-render)))
              hidepass-patterns)))


(defun hidepass-on ()
  "Font-lock configuration and refresh."
  ;; 1. Setup properties
  (add-to-list (make-local-variable 'font-lock-extra-managed-props)
               'display)
  ;; 2. Mark the matcher symbol as multiline (Safe to do repeatedly)
  (when hidepass-multiline-patterns
    (put 'hidepass--match-multiline-block 'font-lock-multiline t))

  ;; 3. Add keywords
  (font-lock-add-keywords nil (hidepass-font-lock-keywords) 'prepend)

  ;; Force a hard reset of font-lock
  (font-lock-mode -1)
  (font-lock-mode 1)
  (font-lock-flush)
  (font-lock-ensure))


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
    (hidepass-off)))

(provide 'hidepass)
;;; hidepass.el ends here
