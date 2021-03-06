;;; macos-utils.el --- Open files in external macOS applications
;;
;; Copyright (c) 2012-2017 Jacob Chaffin
;;
;; Author:  Jacob Chaffin <jchaffin@ucla.edu>
;; Home Page: https://github.com/jchaffin/macos-dev-utils.git
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary
;; Provides commands for launching and interacting with
;; external applications and developer tools on macOS >10.12

;;; Code

(defun macos-buffer-file (&optional buffer)
  "Return the quoted name of file BUFFER is visiting, or the default
directory if there is none."
  (let ((buf (or buffer (current-buffer))))
    (shell-quote-argument
     (file-truename
      (cond ((eq major-mode 'dired-mode)
             (if-let ((filename (dired-file-name-at-point)))
                 filename (dired-current-directory t)))
            (t (if-let ((filename (buffer-file-name buf)))
                   filename default-directory)))))))

(defun macos-exec-with-file (&rest cmd)
  "Run the shell CMD with file or directory as its argument. The first
element in CMD may be either an executable name or the path to an
executable or a function which takes the concatenated form of the remaining
elements as its argument."
  (cl-flet ((fcmd (lst) (mapconcat #'identity lst " ")))
    (let ((exec (car cmd))
          (file (macos-buffer-file)))
      (unwind-protect
          (cond ((functionp exec) (funcall exec file))
                ((executable-find exec) (shell-command (fcmd (list (fcmd cmd) file))))
                (t (error "executable %s not found" exec)))
        (kill-buffer-if-not-modified (get-file-buffer file))))))

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

;;;###autoload
(defun iterm-focus ()
  "If there is an active iTerm session, then
return focus to that session. Otherwise launch iTerm."
  (interactive)
  (do-applescript
   "do shell script \"open -a iTerm\"\n"))

;;;###autoload
(defun iterm-cwd (file)
  "Go to current working directory and focus iTerm."
  (interactive (list (macos-buffer-file)))
  (do-applescript
   (format
    (concat
     "tell application \"iTerm\"\n"
     "  tell the current session of current window\n"
     "  write text \"cd %s\"\n"
     "   end tell\n"
     "end tell\n"
     "do shell script \"open -a iTerm\"\n")
    (file-name-directory file))))

;;;###autoload
(defun iterm-shell-command (command &optional prefix)
  "cd to `default-directory' then run COMMAND in iTerm.
With PREFIX, cd to project root."
  (interactive
   (list (read-shell-command "iTerm Command: ") current-prefix-arg))
  (let* ((dir (if prefix (project-root) default-directory))
         ;; if COMMAND is empty, just change directory
         (cmd (format "cd %s ;%s" dir command)))
    (do-applescript
     (format
      (concat
       "tell application \"iTerm\"\n"
       "  activate\n"
       "  set _session to current session of current window\n"
       "  tell _session\n"
       "    set comma(macroexpand '(macos-make-external-command "iterm" #'iterm-cwd))nd to get the clipboard\n"
       "    write text \"%s\"\n"
       "  end tell\n"
       "end tell")
      cmd))))

(eval-and-compile
  (macos-make-external-command "default" "open")
  (macos-make-external-command "pdfpen" "open" "-b" "com.smileonmymac.pdfpen-setapp")
  (macos-make-external-command "atom")
  (macos-make-external-command "bbedit")
  (macos-make-external-command "coda")
  (macos-make-external-command "sublime-text" "subl")
  (macos-make-external-command "vscode" "code")
  (macos-make-external-command "tower" "gittower"))

(defvar macos-open-with-command-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "o") #'open-with-default)
     (define-key map (kbd "s") #'open-with-sublime-text)
     (define-key map (kbd "a") #'open-with-atom)
     (define-key map (kbd "p") #'open-with-pdfpen)
     (define-key map (kbd "t") #'open-with-iterm)
     (define-key map (kbd "v") #'open-with-vscode)
     (define-key map (kbd "b") #'open-with-bbedit)
     (define-key map (kbd "c") #'open-with-coda)
     (define-key map (kbd "t") #'open-with-tower)
     map)
   "Keymap for macOS open commands")
(fset 'macos-open-with-command-map macos-open-with-command-map)

(provide 'macos-utils)
;; macos-utils.el ends here
