;;; grove-profiles-test.el --- Tests for grove profile functionality -*- lexical-binding: t -*-

;; Copyright 2026 Jonathan Chu

;;; Commentary:

;; Tests covering grove--active-directory, grove--active-cache,
;; grove--profile-for-file, and grove-switch-profile.

;;; Code:

(require 'ert)
(require 'grove-core)
(require 'grove-link)
(require 'grove)

(defmacro grove-test--with-profiles (profiles &rest body)
  "Run BODY with PROFILES bound and all profile state reset."
  (declare (indent 1))
  `(let ((grove-profiles ,profiles)
         (grove--active-profile nil)
         (grove--profile-caches (make-hash-table :test #'eq))
         (grove--cache (make-hash-table :test #'equal)))
     ,@body))

;;; grove--active-directory

(ert-deftest grove--active-directory-falls-back-to-grove-directory ()
  (grove-test--with-profiles nil
    (let ((grove-directory "/tmp/my-notes"))
      (should (string= (grove--active-directory)
                       (file-name-as-directory (expand-file-name "/tmp/my-notes")))))))

(ert-deftest grove--active-directory-returns-nil-when-unconfigured ()
  (grove-test--with-profiles nil
    (let ((grove-directory nil))
      (should-not (grove--active-directory)))))

(ert-deftest grove--active-directory-returns-profile-directory ()
  (let ((dir (make-temp-file "grove-profile" t)))
    (unwind-protect
        (grove-test--with-profiles `((work :directory ,dir))
          (setq grove--active-profile 'work)
          (should (string= (grove--active-directory)
                           (file-name-as-directory (expand-file-name dir)))))
      (delete-directory dir t))))

(ert-deftest grove--active-directory-errors-on-unknown-profile ()
  (grove-test--with-profiles '((work :directory "/tmp/work"))
    (setq grove--active-profile 'nonexistent)
    (should-error (grove--active-directory) :type 'user-error)))

;;; grove--active-cache

(ert-deftest grove--active-cache-returns-grove--cache-when-no-profile ()
  (grove-test--with-profiles nil
    (should (eq (grove--active-cache) grove--cache))))

(ert-deftest grove--active-cache-returns-separate-cache-per-profile ()
  (grove-test--with-profiles '((personal :directory "/tmp/personal")
                               (work     :directory "/tmp/work"))
    (setq grove--active-profile 'personal)
    (let ((personal-cache (grove--active-cache)))
      (setq grove--active-profile 'work)
      (let ((work-cache (grove--active-cache)))
        (should-not (eq personal-cache work-cache))
        (should-not (eq personal-cache grove--cache))))))

(ert-deftest grove--active-cache-creates-cache-lazily ()
  (grove-test--with-profiles '((work :directory "/tmp/work"))
    (setq grove--active-profile 'work)
    (should-not (gethash 'work grove--profile-caches))
    (let ((cache (grove--active-cache)))
      (should (hash-table-p cache))
      (should (eq cache (gethash 'work grove--profile-caches))))))

;;; grove--profile-for-file

(ert-deftest grove--profile-for-file-returns-matching-profile ()
  (let ((dir (make-temp-file "grove-profile" t)))
    (unwind-protect
        (grove-test--with-profiles `((work :directory ,dir))
          (should (eq (grove--profile-for-file (expand-file-name "note.org" dir))
                      'work)))
      (delete-directory dir t))))

(ert-deftest grove--profile-for-file-returns-nil-when-no-match ()
  (grove-test--with-profiles '((work :directory "/tmp/work-notes"))
    (should-not (grove--profile-for-file "/tmp/other/note.org"))))

(ert-deftest grove--profile-for-file-returns-nil-when-no-profiles ()
  (grove-test--with-profiles nil
    (should-not (grove--profile-for-file "/tmp/any/note.org"))))

;;; grove-switch-profile

(ert-deftest grove-switch-profile-sets-active-profile ()
  (let ((dir (make-temp-file "grove-profile" t)))
    (unwind-protect
        (grove-test--with-profiles `((personal :directory ,dir)
                                     (work     :directory ,dir))
          (grove-switch-profile "work")
          (should (eq grove--active-profile 'work)))
      (delete-directory dir t))))

;;; grove-link-follow with active profile

(ert-deftest grove-link-follow-creates-note-in-active-profile-directory ()
  (let ((dir (make-temp-file "grove-vault" t)))
    (unwind-protect
        (grove-test--with-profiles `((personal :directory ,dir))
          (setq grove--active-profile 'personal)
          (cl-letf (((symbol-function 'grove-link--resolve) (lambda (_) nil))
                    ((symbol-function 'grove-link--profile-with-title) (lambda (_) nil))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (grove-link-follow "New Note")
            (should (buffer-file-name))
            (should (string-prefix-p (file-name-as-directory (expand-file-name dir))
                                     (buffer-file-name)))
            (when (buffer-live-p (current-buffer))
              (kill-buffer (current-buffer)))))
      (delete-directory dir t))))

(provide 'grove-profiles-test)
;;; grove-profiles-test.el ends here
