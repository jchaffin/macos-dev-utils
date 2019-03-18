;;; macos-open-with.el --- Open files in external macOS applications
;;
;; Copyright (c) 2019 Jacob Chaffin
;;
;; Author:  Jacob Chaffin <jchaffin@ucla.edu>
;; Home Page: https://github.com/jchaffin/macos-dev-utils.git
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;; Open files and buffers in external macOS applications.

;;; Code:
(defun macos-exec-buffer-file ()
  (shell-quote-argument
   (file-truename
    (cond ((eq major-mode 'dired-mode)
           (if-let ((filename (dired-file-name-at-point)))
               filename (dired-current-directory t)))
          (t (or buffer-file-name (getenv "HOME")))))))

(defun macos-exec-with-file (&rest cmd)
  "Run the shell CMD with file or directory as its argument."
  (let ((exec (car cmd))
        (file (macos-exec-buffer-file)))
    (if (executable-find exec)
        (shell-command
         (mapconcat #'identity (append cmd (list file)) " "))
      (message "executable %s not found" exec))))



(defmacro macos-make-external-command (editor &rest executable)
  "Create an open with editor function for an application
named EDITOR with executable name or path EXECUTABLE.
If second argument is not given, then the binary will
be set to the value of editor."
  `(defun ,(intern (concat "open-with-" editor)) ()
      ,(format "Open with %s" editor)
     (interactive)
     (let ((exec (list ,@(or executable editor))))
       (apply #'macos-exec-with-file exec))))

(eval-and-compile
  (macos-make-external-command "default" "open")
  (macos-make-external-command "pdfpen" "open" "-b" "com.smileonmymac.pdfpen-setapp")
  (macos-make-external-command "atom")
  (macos-make-external-command "bbedit")
  (macos-make-external-command "coda")
  (macos-make-external-command "sublime-text" "subl")
  (macos-make-external-command "code" "vscode")
  (macos-make-external-command "tower" "gittower"))

 (provide 'macos-open-with)
