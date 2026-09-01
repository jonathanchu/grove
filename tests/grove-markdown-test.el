;;; grove-markdown-test.el --- Tests for Markdown notes -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;;; Commentary:

;; Coverage for reading Markdown notes: front matter, ATX headings,
;; tags, and Org/Markdown vaults sharing one root.

;;; Code:

(require 'ert)
(require 'grove-core)

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

(provide 'grove-markdown-test)
;;; grove-markdown-test.el ends here
