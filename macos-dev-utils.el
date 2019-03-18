;;; macos-dev-utils.el --- Open files in external macOS applications
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

(defgroup macos nil
  "Customization group for macOS."
  :group 'environment
  :prefix "macos-")

(declare-function 'open-with-default "macos-open-with.el")
(declare-function 'open-with-atom "macos-open-with.el")
(declare-function 'open-with-bbedit "macos-open-with.el")
(declare-function 'open-with-coda "macos-open-with.el")
(declare-function 'open-with-sublime-text "macos-open-with.el")
(declare-function 'open-with-vscode "macos-open-with.el")
(declare-function 'open-with-tower "macos-open-with.el")
(declare-function 'open-with-pdfpen "macos-open-with.el")

(require 'macos-open-with)
(require 'macos-iterm)

(defvar macos-open-with-command-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "o") #'open-with-default)
     (define-key map (kbd "s") #'open-with-sublime-text)
     (define-key map (kbd "a") #'open-with-atom)
     (define-key map (kbd "p") #'open-with-pdfpen)
     (define-key map (kbd "v") #'open-with-vscode)
     (define-key map (kbd "b") #'open-with-bbedit)
     (define-key map (kbd "c") #'open-with-coda)
     (define-key map (kbd "t") #'open-with-tower)
     map)
   "Command map for `macos-open-with'")

(fset 'macos-open-with-command-map macos-open-with-command-map)


(provide 'macos-dev-utils)
;; macos-dev-utils.el ends here
