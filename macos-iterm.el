;;; macos-iterm.el --- Open files in external macOS applications
;;
;; Copyright (c) 2012-2017 Jacob Chaffin
;;
;; Author:  Jacob Chaffin <jchaffin@ucla.edu>
;; Home Page: https://github.com/jchaffin/macos-dev-utils.git
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;; Provides functions for launching or toggling between a terminal session in
;; Iterm.app at certain locations directed by Emacs.
;; Based on https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/


;;; Code:
(defun get-file-dir-or-home ()
  "If inside a file buffer, return the directory, else return home."
  (cond ((eq major-mode 'dired-mode) dired-directory)
        ((stringp buffer-file-name)
         (file-name-directory buffer-file-name))
        (t (directory-file-name default-directory))))

;;;###autoload
(defun iterm-goto-filedir-or-home ()
  "Go to current working directory and focus iTerm."
  (interactive)
  (do-applescript
   (concat
    "tell application \"iTerm\"\n"
    "  tell the current session of current window\n"
    (format "  write text \"cd %s\"\n" (get-file-dir-or-home))
    "   end tell\n"
    "end tell\n"
    "do shell script \"open -a iTerm\"\n")))

;;;###autoload
(defun iterm-focus ()
  "If there is an active iTerm session, then
return focus to that session. Otherwise launch iTerm."
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

;;;###autoload
(defun iterm-shell-command (command &optional prefix)
  "cd to `default-directory' then run COMMAND in iTerm.
With PREFIX, cd to project root."
  (interactive (list (read-shell-command
                      "iTerm Shell Command: ")
                     current-prefix-arg))
  (let* ((dir (if prefix (project-root)
                default-directory))
         ;; if COMMAND is empty, just change directory
         (cmd (format "cd %s ;%s" dir command)))
    (do-applescript
     (format
      (concat
       "tell application \"iTerm\"\n"
       "  activate\n"
       "  set _session to current session of current window\n"
       "  tell _session\n"
       "    set command to get the clipboard\n"
       "    write text \"%s\"\n"
       "  end tell\n"
       "end tell")
      cmd))))

(defvar macos-iterm-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'iterm-goto-filedir-or-home)
    (define-key map (kbd "f") #'iterm-focus)
    (define-key map (kbd "s") #'iterm-shell-command)
    map)
  "Keymap for macos iterm.")

(fset 'macos-iterm-command-map macos-iterm-command-map)


(provide 'macos-iterm)

;; macos-iterm.el ends here.
