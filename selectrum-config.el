;;; selectrum-config.el --- Configuration for Selectrum. -*- lexical-binding: t -*-

;; Copyright (C) 2020 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 12 Aug 2020
;; Homepage: https://github.com/raxod502/selectrum
;; Keywords: extensions
;; Package-Requires: ((emacs "25.1") (selectrum "2.0"))
;; SPDX-License-Identifier: MIT
;; Version: 2.0

;;; Commentary:

;; This file contains configurations for Selectrum. Those can be used
;; to automatically setup integration with complementary packages.

;;;###autoload
(defun selectrum-config-embark ()
  (if selectrum-mode
      ...
    ...))

(provide 'selectrum-config)

;;; selectrum-config.el ends here

