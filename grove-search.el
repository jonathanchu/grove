;;; grove-search.el --- Search integration for grove -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/grove

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Search integration for grove.  Provides full-text ripgrep search
;; and note finding, with optional consult integration for live
;; preview and narrowing.

;;; Code:

(require 'grove-core)
(require 'subr-x)

(declare-function consult--grep "consult")
(declare-function consult--ripgrep-make-builder "consult")
(declare-function consult--find "consult")
(defvar consult-ripgrep-args)
(defvar consult-async-split-style)

;;;; Full-text search

;;;###autoload
(defun grove-search (&optional initial)
  "Search note contents in the vault.
With optional INITIAL input string.  Uses `consult-ripgrep' when
available, otherwise falls back to `grep'."
  (interactive)
  (grove--ensure-directory)
  (if (featurep 'consult)
      (grove-search--consult-ripgrep "Grove search" initial)
    (grove-search--grep initial)))

(defun grove-search--consult-ripgrep (prompt &optional initial split-style)
  "Search the vault with consult and ripgrep.
PROMPT is the minibuffer prompt and INITIAL the initial input, which
Consult reads as an Emacs regexp.  SPLIT-STYLE overrides
`consult-async-split-style' for this search."
  (let ((consult-ripgrep-args
         (concat consult-ripgrep-args " "
                 (string-join (grove--rg-glob-args) " ")))
        (consult-async-split-style (or split-style consult-async-split-style)))
    (consult--grep prompt #'consult--ripgrep-make-builder
                   (grove--active-directory) initial)))

(defun grove-search--grep (&optional initial)
  "Search vault with grep.  INITIAL is the initial input."
  (let ((pattern (read-string "Grove search: " initial)))
    (grep (format "rg --no-heading --line-number %s %s %s"
                  (mapconcat #'shell-quote-argument (grove--rg-glob-args) " ")
                  (shell-quote-argument pattern)
                  (shell-quote-argument (grove--active-directory))))))

;;;; Find note by title

;;;###autoload
(defun grove-find ()
  "Find a note by title using `completing-read' over cached titles."
  (interactive)
  (grove--ensure-directory)
  (grove--refresh-cache)
  (let* ((titles (grove--note-titles))
         (choice (completing-read "Find note: " (mapcar #'car titles) nil t)))
    (when-let ((path (cdr (assoc choice titles))))
      (find-file path))))

;;;; Tag search

(defun grove-search--normalize-tag (tag)
  "Normalize TAG input from the minibuffer.
Accepts bare tag names plus #tag and :tag: syntax, and returns the
plain tag name."
  (let ((normalized (string-trim tag)))
    (setq normalized (replace-regexp-in-string "\\`#+" "" normalized))
    (setq normalized (replace-regexp-in-string "\\`:+\\|:+\\'" "" normalized))
    normalized))

(defun grove-search--tag-pattern (tag &optional syntax)
  "Build a search pattern for TAG.
SYNTAX defaults to `rg', producing a pattern handed straight to
ripgrep.  With `emacs' the pattern uses Emacs regexp syntax, which is
what Consult expects: it reads its input as an Emacs regexp and
converts it to the backend syntax itself."
  (let* ((normalized (grove-search--normalize-tag tag))
         (quoted (regexp-quote normalized)))
    (when (string-empty-p normalized)
      (user-error "Tag cannot be empty"))
    (if (eq syntax 'emacs)
        (format "\\(#%s\\b\\|:%s:\\)" quoted quoted)
      (format "(#%s\\b|:%s:)" quoted quoted))))

;;;###autoload
(defun grove-search-tag (&optional initial)
  "Search for notes by tag.
With optional INITIAL input string.  Searches for both org-style
:tag: and inline #tag patterns."
  (interactive)
  (grove--ensure-directory)
  (let ((tag (or initial (read-string "Tag: "))))
    (if (featurep 'consult)
        ;; Force the `none' split style: the default `perl' style takes its
        ;; separator from the leading punctuation character and would
        ;; truncate the pattern at its first `#'.
        (grove-search--consult-ripgrep
         "Grove tags" (grove-search--tag-pattern tag 'emacs) 'none)
      (let ((pattern (grove-search--tag-pattern tag)))
        (grep (format "rg --no-heading --line-number %s %s %s"
                      (mapconcat #'shell-quote-argument (grove--rg-glob-args) " ")
                      (shell-quote-argument pattern)
                      (shell-quote-argument (grove--active-directory))))))))

(provide 'grove-search)
;;; grove-search.el ends here
