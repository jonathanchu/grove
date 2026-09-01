;;; grove-markdown-test.el --- Tests for Markdown notes -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;;; Commentary:

;; Coverage for reading Markdown notes: front matter, ATX headings,
;; tags, and Org/Markdown vaults sharing one root.

;;; Code:

(require 'ert)
(require 'grove-core)
(require 'grove-capture)
(require 'grove-link)
(require 'grove-daily)
(require 'grove)

(defmacro grove-markdown-test--with-note (extension content &rest body)
  "Write CONTENT to a temporary note with EXTENSION and run BODY.
BODY is evaluated with `file' bound to the note's path."
  (declare (indent 2))
  `(let ((file (make-temp-file "grove-md" nil ,extension ,content)))
     (unwind-protect (progn ,@body)
       (delete-file file))))

(ert-deftest grove-parse-note-reads-markdown-front-matter ()
  (grove-markdown-test--with-note ".md"
      "---\ntitle: Front Matter Title\ntags: [alpha, beta]\n---\n\nBody\n"
    (let ((meta (grove--parse-note file)))
      (should (equal (plist-get meta :title) "Front Matter Title"))
      (should (equal (plist-get meta :tags) '("alpha" "beta"))))))

(ert-deftest grove-parse-note-reads-markdown-block-sequence-tags ()
  "Obsidian writes tags as a YAML block sequence as often as a flow one."
  (grove-markdown-test--with-note ".md"
      "---\ntitle: Block\ntags:\n  - alpha\n  - \"beta\"\n  - #gamma\n---\n\nBody\n"
    (should (equal (plist-get (grove--parse-note file) :tags)
                   '("alpha" "beta" "gamma")))))

(ert-deftest grove-parse-note-stops-block-tags-at-the-next-key ()
  (grove-markdown-test--with-note ".md"
      "---\ntags:\n  - alpha\nauthor: someone\n---\n\n# Heading\n"
    (let ((meta (grove--parse-note file)))
      (should (equal (plist-get meta :tags) '("alpha")))
      (should (equal (plist-get meta :title) "Heading")))))

(ert-deftest grove-parse-note-uses-markdown-heading-as-title ()
  (grove-markdown-test--with-note ".md" "# Heading Title\n\n## Subheading\n\nBody\n"
    (should (equal (plist-get (grove--parse-note file) :title) "Heading Title"))))

(ert-deftest grove-parse-note-prefers-front-matter-over-heading ()
  (grove-markdown-test--with-note ".md"
      "---\ntitle: Front Matter\n---\n\n# Heading\n"
    (should (equal (plist-get (grove--parse-note file) :title) "Front Matter"))))

(ert-deftest grove-parse-note-falls-back-to-markdown-filename ()
  (grove-markdown-test--with-note ".md" "No title anywhere.\n"
    (should (equal (plist-get (grove--parse-note file) :title)
                   (file-name-sans-extension (file-name-nondirectory file))))))

(ert-deftest grove-parse-note-ignores-markdown-headings-as-hashtags ()
  "A heading puts whitespace after the hash; a tag does not."
  (grove-markdown-test--with-note ".md" "# Heading\n\n## Sub\n\nBody with #real-tag\n"
    (should (equal (plist-get (grove--parse-note file) :tags) '("real-tag")))))

(ert-deftest grove-parse-note-collects-wikilinks-from-markdown ()
  (grove-markdown-test--with-note ".md"
      "# Title\n\nSee [[Other Note]] and [[https://example.com]].\n"
    (should (equal (plist-get (grove--parse-note file) :links) '("Other Note")))))

(ert-deftest grove-parse-note-ignores-a-horizontal-rule-in-the-body ()
  "Front matter only counts when it opens on the very first line."
  (grove-markdown-test--with-note ".md" "# Real Title\n\n---\n\ntitle: not this\n"
    (should (equal (plist-get (grove--parse-note file) :title) "Real Title"))))

(ert-deftest grove-refresh-cache-holds-org-and-markdown-together ()
  (let* ((grove-profiles nil)
         (grove--active-profile nil)
         (grove-directory (make-temp-file "grove-vault" t))
         (grove--cache (make-hash-table :test #'equal))
         (org-note (expand-file-name "note.org" grove-directory))
         (md-note (expand-file-name "note.md" grove-directory))
         (upper-md (expand-file-name "UPPER.MD" grove-directory)))
    (unwind-protect
        (progn
          (with-temp-file org-note (insert "#+title: Org Note\n"))
          (with-temp-file md-note (insert "---\ntitle: Md Note\n---\n"))
          (with-temp-file upper-md (insert "# Upper Md\n"))
          (grove--refresh-cache)
          (should (= (hash-table-count grove--cache) 3))
          (should (equal (plist-get (gethash org-note grove--cache) :title)
                         "Org Note"))
          (should (equal (plist-get (gethash md-note grove--cache) :title)
                         "Md Note"))
          (should (equal (plist-get (gethash upper-md grove--cache) :title)
                         "Upper Md")))
      (delete-directory grove-directory t))))

(ert-deftest grove-parse-note-still-reads-org-keywords ()
  "The Org path must be untouched by format dispatch."
  (grove-markdown-test--with-note ".org"
      "#+title: Org Note\n#+filetags: :alpha:beta:\n\nSee [[Other]] #gamma\n"
    (let ((meta (grove--parse-note file)))
      (should (equal (plist-get meta :title) "Org Note"))
      (should (equal (plist-get meta :tags) '("alpha" "beta" "gamma")))
      (should (equal (plist-get meta :links) '("Other"))))))

(ert-deftest grove-note-header-round-trips-markdown-titles ()
  "A title grove writes must be a title grove reads back.
Front matter is YAML, so a colon or a quote in the title has to be
quoted and escaped or other tools misread the file."
  (let ((grove-default-format 'md))
    (dolist (title '("Project: Alpha" "Say \"Hello\"" "Back\\slash" "Plain"))
      (grove-markdown-test--with-note ".md" (grove--note-header title)
        (should (equal (plist-get (grove--parse-note file) :title) title))))))

(ert-deftest grove-new-note-filename-follows-the-default-format ()
  (should (equal (let ((grove-default-format 'org))
                   (grove--new-note-filename "Some Note"))
                 "some-note.org"))
  (should (equal (let ((grove-default-format 'md))
                   (grove--new-note-filename "Some Note"))
                 "some-note.md")))

(ert-deftest grove-note-header-rejects-an-unknown-format ()
  (let ((grove-default-format 'rst))
    (should-error (grove--note-header "Note") :type 'user-error)))

(ert-deftest grove-capture-finalize-writes-the-default-format ()
  (let* ((grove-profiles nil)
         (grove--active-profile nil)
         (grove-directory (make-temp-file "grove-vault" t))
         (grove-default-format 'md))
    (unwind-protect
        (with-temp-buffer
          (insert "Captured Note
Body line
")
          (grove-capture-finalize)
          (let ((path (buffer-file-name)))
            (should (equal (file-name-nondirectory path) "captured-note.md"))
            (should (equal (buffer-string)
                           "---\ntitle: \"Captured Note\"\n---\n\nBody line\n"))
            (kill-buffer (current-buffer))))
      (delete-directory grove-directory t))))

(ert-deftest grove-link-follow-creates-the-default-format ()
  (let* ((grove-profiles nil)
         (grove--active-profile nil)
         (grove-directory (make-temp-file "grove-vault" t))
         (grove-default-format 'md))
    (unwind-protect
        (cl-letf (((symbol-function 'grove-link--resolve) (lambda (_title) nil))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (grove-link-follow "New Note")
          (should (equal (file-name-nondirectory (buffer-file-name)) "new-note.md"))
          (should (equal (buffer-string) "---\ntitle: \"New Note\"\n---\n\n"))
          (kill-buffer (current-buffer)))
      (delete-directory grove-directory t))))

(ert-deftest grove-daily-opens-an-existing-note-in-another-format ()
  "A daily note already written as Markdown must not be shadowed.
`grove-daily' built one fixed path from the default format, so an
existing 2026-08-31.md went unseen and a second .org note was created
beside it."
  (let* ((grove-profiles nil)
         (grove--active-profile nil)
         (grove-directory (make-temp-file "grove-vault" t))
         (grove-default-format 'org)
         (existing (expand-file-name
                    (concat (format-time-string grove-daily-format) ".md")
                    (grove--daily-path))))
    (unwind-protect
        (progn
          (with-temp-file existing (insert "---\ntitle: Already Here\n---\n"))
          (grove-daily)
          (should (equal (buffer-file-name) existing))
          (should (equal (buffer-string) "---\ntitle: Already Here\n---\n"))
          (kill-buffer (current-buffer))
          (should (= (length (directory-files (file-name-directory existing)
                                              nil "\\`[^.]"))
                     1)))
      (delete-directory grove-directory t))))

(ert-deftest grove-daily-creates-the-default-format ()
  (let* ((grove-profiles nil)
         (grove--active-profile nil)
         (grove-directory (make-temp-file "grove-vault" t))
         (grove-default-format 'md))
    (unwind-protect
        (progn
          (grove-daily)
          (should (equal (file-name-extension (buffer-file-name)) "md"))
          (should (string-prefix-p "---\ntitle: " (buffer-string)))
          (should (string-match-p "^date: " (buffer-string)))
          (kill-buffer (current-buffer)))
      (delete-directory grove-directory t))))

(ert-deftest grove-turn-on-activates-in-markdown-notes ()
  "Activation keys on the file, not the major mode.
Without `markdown-mode' installed a .md note opens in `fundamental-mode',
and a mode test would leave wikilinks dead in exactly the vaults this
support is for."
  (let* ((grove-profiles nil)
         (grove--active-profile nil)
         (grove-directory (make-temp-file "grove-vault" t))
         (note (expand-file-name "note.md" grove-directory))
         (other (expand-file-name "notes.txt" grove-directory)))
    (unwind-protect
        (progn
          (with-temp-file note (insert "# Note\n"))
          (with-temp-file other (insert "not a note\n"))
          (find-file note)
          (fundamental-mode)
          (grove--turn-on)
          (should grove-mode)
          (kill-buffer (current-buffer))
          (find-file other)
          (grove--turn-on)
          (should-not grove-mode)
          (kill-buffer (current-buffer)))
      (delete-directory grove-directory t))))

(provide 'grove-markdown-test)
;;; grove-markdown-test.el ends here
