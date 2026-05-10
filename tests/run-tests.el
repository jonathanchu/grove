;;; run-tests.el --- Load and run all Grove tests -*- lexical-binding: t -*-

;; Copyright 2026 Guilherme Thomazi Bonicontro

;;; Commentary:

;; Loads every *-test.el file in this directory, then runs the full ERT suite.

;;; Code:

(require 'ert)

(let* ((this-file (or load-file-name buffer-file-name))
       (tests-directory (file-name-directory this-file)))
  (dolist (file (directory-files tests-directory t "-test\\.el\\'"))
    (load file nil 'nomessage)))

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
