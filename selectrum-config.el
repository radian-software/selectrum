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

(defvar selectrum--old-minibuffer-default-add-function nil)
(defun selectrum-minibuffer-default-add-function ()
  "Meant to be used as `minibuffer-default-add-function'."
  (with-selected-window (minibuffer-selected-window)
    (delete-dups
     (delq nil
           (list (thing-at-point 'symbol)
                 (thing-at-point 'list)
                 (ffap-guesser)
                 (thing-at-point-url-at-point))))))

(defun selectrum-setup-minibuffer-default-add-function ()
  (if selectrum-mode
      (progn
        (setq selectrum--old-minibuffer-default-add-function
              minibuffer-default-add-function)
        (setq minibuffer-default-add-function
              #'selectrum-minibuffer-default-add-function))
    (when (equal (default-value 'minibuffer-default-add-function)
                 #'selectrum-minibuffer-default-add-function)
      (setq-default minibuffer-default-add-function
                    selectrum--old-minibuffer-default-add-function))))

(defun selectrum-complete-path-at-point ()
  "Meant to be used for `completion-at-point-functions'."
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn
                   (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table))))

(defun selectrum-setup-completion-at-point-functions ()
  (if selectrum-mode
      (add-hook 'completion-at-point-functions
                #'selectrum-complete-path-at-point 'append)
    (remove-hook 'completion-at-point-functions
                 #'selectrum-complete-path-at-point)))


(defun selectrum-setup-embark ()
  (with-eval-after-load 'embark
    (if selectrum-mode
        ...
      ...)))


(provide 'selectrum-config)

;;; selectrum-config.el ends here

