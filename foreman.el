;;; foreman.el --- Interact with Foreman from Emacs

;; Copyright (c) 2011 Justin Talbott <justin@waymondo.com>

;; Author: Justin Talbott <justin@waymondo.com>
;; URL: http://github.com/waymondo/
;; Keywords: foreman ruby
;; Created: 07 Jun 2012
;; Version: 0.0.1

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
(setq comint-buffer-maximum-size 10240) ;; set maximum-buffer size for shell-mode
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer) ;; truncate shell buffer to comint-buffer-maximum-size.

(defun foreman-start ()
  "Run foreman start for the current project."
  (interactive)
  (let ((procfile-dir (locate-procfile)))
    (if (= (length procfile-dir) 0)
        (message "Procfile not found or missing.")
      (defun toggle-read-only-and-unhook ()
        (interactive)
        (toggle-read-only 1)
        (remove-hook 'shell-mode-hook 'toggle-read-only-and-unhook)
        )
      (add-hook 'shell-mode-hook 'toggle-read-only-and-unhook)
      (async-shell-command (format "cd %s && foreman start" (shell-quote-argument procfile-dir)) "*Foreman*")
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
