;;; grove-review-test.el --- Regression tests for review findings -*- lexical-binding: t -*-

;; Copyright 2026 Guilherme Thomazi Bonicontro

;;; Commentary:

;; Regression coverage for issues found during code review.

;;; Code:

(require 'ert)
(require 'grove-core)
(require 'grove-backlink)
(require 'grove-link)
(require 'grove-inbox)
(require 'grove-tree)
(require 'grove-ui)
(require 'subr-x)

(ert-deftest grove-parse-note-reads-beyond-4kb ()
  (let ((file (make-temp-file "grove-large" nil ".org"
                              (concat "#+title: Large note\n\n"
                                      (make-string 5000 ?a)
                                      "\n#late-tag\n[[Late Link]]\n"))))
    (unwind-protect
        (let ((meta (grove--parse-note file)))
          (should (equal (plist-get meta :tags) '("late-tag")))
          (should (equal (plist-get meta :links) '("Late Link"))))
      (delete-file file))))

(ert-deftest grove-parse-note-keeps-colon-titles-as-wikilinks ()
  (let ((file (make-temp-file "grove-colon" nil ".org"
                              "#+title: Link test\n\n[[Project: Alpha]]\n[[https://example.com]]\n")))
    (unwind-protect
        (should (equal (plist-get (grove--parse-note file) :links)
                       '("Project: Alpha")))
      (delete-file file))))

(ert-deftest grove-link-fontify-allows-colons-in-note-titles ()
  (with-temp-buffer
    (insert "[[Project: Alpha]] [[https://example.com]]")
    (goto-char (point-min))
    (grove-link--fontify (point-max))
    (goto-char (point-min))
    (search-forward "[[Project: Alpha]]")
    (should (equal (get-text-property (match-beginning 0) 'grove-link-target)
                   "Project: Alpha"))
    (search-forward "[[https://example.com]]")
    (should-not (get-text-property (match-beginning 0) 'grove-link-target))))

(ert-deftest grove-link-follow-creates-unique-file-on-filename-collision ()
  (let* ((grove-directory (make-temp-file "grove-vault" t))
         (existing (expand-file-name "foo.org" grove-directory)))
    (unwind-protect
        (progn
          (with-temp-file existing
            (insert "#+title: Existing\n\n"))
          (cl-letf (((symbol-function 'grove-link--resolve) (lambda (_title) nil))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (grove-link-follow "Foo!")
            (should (buffer-file-name))
            (should (string= (file-name-nondirectory (buffer-file-name)) "foo-1.org"))
            (should (string= (buffer-string) "#+title: Foo!\n\n")))
          (when (buffer-live-p (current-buffer))
            (kill-buffer (current-buffer)))
          (with-temp-buffer
            (insert-file-contents existing)
            (should (string= (buffer-string) "#+title: Existing\n\n"))))
      (delete-directory grove-directory t))))

(ert-deftest grove-inbox-review-renders-unlinked-section ()
  (let ((grove-directory (make-temp-file "grove-vault" t)))
    (unwind-protect
        (let ((grove--cache (make-hash-table :test #'equal)))
          (puthash "/tmp/a.org" (list :title "A" :tags nil) grove--cache)
          (puthash "/tmp/b.org" (list :title "B" :tags '("tag")) grove--cache)
          (cl-letf (((symbol-function 'grove--refresh-cache) #'ignore)
                    ((symbol-function 'grove--ensure-directory) #'ignore)
                    ((symbol-function 'grove-inbox--unlinked-notes)
                     (lambda () '(("B" . "/tmp/b.org")))))
            (grove-inbox-review)
            (with-current-buffer grove-inbox-buffer-name
              (should (string-match-p "No backlinks (1)" (buffer-string)))
              (should (string-match-p "B" (buffer-string))))
            (kill-buffer grove-inbox-buffer-name)))
      (delete-directory grove-directory t))))

(ert-deftest grove-tree-refresh-rebuilds-expanded-children ()
  (let* ((grove-directory (make-temp-file "grove-vault" t))
         (subdir (expand-file-name "sub" grove-directory))
         (file (expand-file-name "note.org" subdir)))
    (unwind-protect
        (progn
          (make-directory subdir)
          (with-temp-file file
            (insert "#+title: Note\n"))
          (with-current-buffer (get-buffer-create grove-tree-buffer-name)
            (grove-tree-mode)
            (clrhash grove-tree--expanded)
            (puthash subdir t grove-tree--expanded)
            (grove-tree-refresh)
            (let ((items nil))
              (ewoc-map (lambda (node) (push (grove-tree-node-name node) items))
                        grove-tree--ewoc)
              (should (member "sub" items))
              (should (member "note" items))))
          (kill-buffer grove-tree-buffer-name))
      (delete-directory grove-directory t))))

(ert-deftest grove-tree-tracks-current-file-via-hooks ()
  (let* ((grove-directory (make-temp-file "grove-vault" t))
         (file-a (expand-file-name "a.org" grove-directory))
         (file-b (expand-file-name "b.org" grove-directory)))
    (unwind-protect
        (progn
          (with-temp-file file-a
            (insert "#+title: A\n"))
          (with-temp-file file-b
            (insert "#+title: B\n"))
          (with-current-buffer (get-buffer-create grove-tree-buffer-name)
            (grove-tree-mode)
            (setq grove-tree--ewoc t))
          (cl-letf (((symbol-function 'ewoc-refresh) (lambda (&rest _) nil))
                    ((symbol-function 'hl-line-highlight) (lambda (&rest _) nil)))
            (grove-tree--enable-tracking)
            (find-file file-a)
            (grove-tree--track-current-file)
            (with-current-buffer grove-tree-buffer-name
              (should (equal grove-tree--current-file file-a)))
            (find-file file-b)
            (grove-tree--track-current-file)
            (with-current-buffer grove-tree-buffer-name
              (should (equal grove-tree--current-file file-b))))
          (grove-tree--disable-tracking)
          (kill-buffer grove-tree-buffer-name)
          (when (buffer-file-name)
            (kill-buffer (current-buffer))))
      (delete-directory grove-directory t))))

(ert-deftest grove-backlink-find-errors-when-ripgrep-is-missing ()
  (let ((grove-directory (make-temp-file "grove-vault" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
          (should-error (grove-backlink--find "Note")
                        :type 'user-error))
      (delete-directory grove-directory t))))

(ert-deftest grove-tree-navigation-advances-past-previewed-entries ()
  "Repeated `n' must reach every entry, not cycle the first few.
Previewing selects the main window, so `grove-tree--set-current-file'
runs with the tree window unselected.  Saving `point' there captured a
stale buffer point while `ewoc-refresh' clobbered the window point,
pinning navigation to the top of the tree."
  (let* ((grove-profiles nil)
         (grove-directory (make-temp-file "grove-nav" t))
         (grove-tree-icons nil))
    (unwind-protect
        (progn
          (dotimes (i 5)
            (with-temp-file (expand-file-name (format "note-%d.org" i)
                                              grove-directory)
              (insert (format "#+title: Note %d\n" i))))
          (grove-open)
          (select-window (get-buffer-window grove-tree-buffer-name))
          (goto-char (point-min))
          (let (points)
            (dotimes (_ 5)
              (grove-tree-next)
              (push (window-point (get-buffer-window grove-tree-buffer-name))
                    points))
            (setq points (nreverse points))
            (should (equal points (sort (copy-sequence points) #'<)))
            (should (= (length (delete-dups (copy-sequence points))) 5))))
      (ignore-errors (grove-close))
      (delete-directory grove-directory t))))

(provide 'grove-review-test)
;;; grove-review-test.el ends here
