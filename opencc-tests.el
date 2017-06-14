;;; opencc-tests.el --- Tests for opencc.el          -*- lexical-binding: t; -*-

;;; Code:

(require 'opencc)

(unless (string= (opencc-string "s2t" "让我们团结起来")
                 "讓我們團結起來")
  (error "opencc-string"))

;; ert
;; benchmark

;;; opencc-tests.el ends here
