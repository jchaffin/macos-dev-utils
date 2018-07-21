;;; macos-open-with.el --- Open files in external macOS applications
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
;; Provides functions for launching or toggling between
;; external text editors at certain locations directed by Emacs.

;;; Code:
(defun macos-open-with-default-app ()
  "Opens the current file buffer with the default
macOS application."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    (shell-quote-argument buffer-file-name)))))


(defun macos-open-with--exec-bin (bin dir file)
  (interactive)
  (let* ((revealpath (if file
                         (concat dir file)
                       dir)))
    (view-mode)
    (view-mode-enable)
    (message (format "Opening %s with %s" revealpath bin))
    (shell-command (concat
                    bin
                    " "
                    revealpath))))


(defun macos-open-with--editor (f &optional bin)
  "Opens current buffer or file at point
in external editor."
  (let* ((path (buffer-file-name))
         (filename-at-point (dired-file-name-at-point))
         ;; Create a full path if filename-at-point is non-nil.
         (filename-at-point (if filename-at-point
                                (expand-file-name filename-at-point)
                              nil))
         dir file)

    (cond (path
           (setq dir (file-name-directory path))
           (setq file (file-name-nondirectory path)))
          (filename-at-point
           ;; if filename-at-point is available from dired.
           (setq dir (file-name-directory filename-at-point))
           (setq file (file-name-nondirectory filename-at-point)))

          (t
           ;; Otherwise,
           (setq dir (expand-file-name default-directory))))
    (if bin
        (funcall-interactively f bin dir file)
      (funcall-interactively f dir file))))


(defalias 'macos-open-with--bin
  (apply-partially
   'macos-open-with--editor
   'macos-open-with--exec-bin))

;;;###autoload
(defun open-with-atom ()
  (interactive)
  (macos-open-with--bin "atom"))

;;;###autoload
(defun open-with-bbedit ()
  (interactive)
  (macos-open-with--bin "bbedit"))

;;;###autoload
(defun open-with-coda ()
  (interactive)
  (macos-open-with--bin "coda"))

;;;###autoload
(defun open-with-sublime-text ()
  (interactive)
  (macos-open-with--bin "subl"))

;;;###autoload
(defun open-with-vscode ()
  (interactive)
  (macos-open-with--bin "code"))

;;;###autoload
(defun open-with-tower ()
  "If inside a file buffer, opens the project
in Tower.app. Else if in a dired buffer, opens the containing
directory."
  (interactive)
  (shell-command
   (concat "gittower" " " (or default-directory dired-directory))))

(defvar macos-open-with-command-map ()
  (let ((map (make-sparse-keymap)))
   (define-key map (kbd "o") #'macos-open-with-default-app)
   (define-key map (kbd "s") #'open-with-sublime-text)
   (define-key map (kbd "a") #'open-with-atom)
   (define-key map (kbd "v") #'open-with-vscode)
   (define-key map (kbd "b") #'open-with-bbedit)
   (define-key map (kbd "c") #'open-with-code)
   (define-key map (kbd "t") #'open-with-tower)
   map)
  (fset macos-open-with-command-map macos-open-with-command-map))

 (provide 'macos-open-with)
