;;; foreman.el --- Interact with Foreman from Emacs

;; Copyright (c) 2011 Justin Talbott <justin@waymondo.com>

;; Author: Justin Talbott <justin@waymondo.com>
;; URL: http://github.com/waymondo/
;; Keywords: foreman ruby
;; Created: 07 Jun 2012
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Install

;; $ cd ~/.emacs.d/vendor
;; $ git clone git://github.com/waymondo/foreman.el.git
;;
;; In your emacs config:
;;
;; (add-to-list 'load-path "~/.emacs.d/vendor/foreman.el")
;; (require 'foreman)

;;; Code:

(eval-when-compile (require 'cl))

(defun foreman-shell-readonly-scroll-and-truncate ()
  (make-local-variable 'comint-scroll-show-maximum-output)
  (make-local-variable 'comint-buffer-maximum-size)
  (setq comint-scroll-show-maximum-output t) ;; always scroll to bottom to show maximum output
  (setq comint-buffer-maximum-size 100) ;; set maximum-buffer size for comint-mode
  (toggle-read-only +1)
  (if (string= procfile-name (buffer-name))
      (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)) ;; truncate out buffer to comint-buffer-maximum-size.
)

(defun foreman-shell-hook ()
  (interactive)
  (if (string= procfile-name (buffer-name))
      (foreman-shell-readonly-scroll-and-truncate)))

(defun foreman-start (&optional name)
  "Run foreman start for the current project."
  (interactive)
  (defvar procfile-name (if (= (length name) 0)
                            "*Foreman*"
                          (concat "*" name "-Foreman*")))
  (let ((procfile-dir (locate-procfile)))
    (if (= (length procfile-dir) 0)
        (message "Procfile not found or missing.")
      (add-hook 'shell-mode-hook 'foreman-shell-hook)
      (async-shell-command (format "cd %s && foreman start" (shell-quote-argument procfile-dir)) procfile-name)
      )))

(defun* locate-procfile (&optional (dir default-directory))
  (let ((has-procfile (directory-files dir nil "^Procfile$"))
        (is-root (equal dir "/")))
    (cond
     (has-procfile dir)
     (is-root
      (print (format
              "No Procfile found in either %s or any parent directory!"
              default-directory))
      nil)
     ((locate-procfile (expand-file-name ".." dir))))))

(provide 'foreman)
;;; foreman.el ends here.
