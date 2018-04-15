;; Emacs Init

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;------------------------------------------------------------------------------
;; General Settings
;;

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa"
                                 . "https://elpa.emacs-china.org/melpa/"))

;; auto insert closing bracket
(electric-pair-mode 1)
;; turn on bracket match highlight
(show-paren-mode 1)

;; turn on highlighting curent line
(global-hl-line-mode 1)

;; Show Line Numbers
;; linum-mode
(global-linum-mode 1) ; always show line numbers
;; global-display-line-numbers-mode -- Emacs 26
;(when (version<= "26.0.50" emacs-version )
;  (global-display-line-numbers-mode))

;; show cursor position within line
(column-number-mode 1)

;; stop creating those backup~ files
(setq make-backup-files nil)


;; make cursor movement keys under right hand's home-row.
(global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
(global-set-key (kbd "M-l") 'forward-char)  ; was downcase-word
(global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
(global-set-key (kbd "M-k") 'next-line) ; was kill-sentence


;; TAB Settings
(progn
  ;; make indentation commands use space only (never tab character)
  (setq-default indent-tabs-mode nil)
  ;; emacs 23.1, 24.2, default to t
  ;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
  )
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8
;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)

;; load  el files
(load "~/.emacs.d/lisp/my-abbrev.el")
(load "~/.emacs.d/lisp/my-alias.el")

;;------------------------------------------------------------------------------
;; Org Mode
;; Turn on syntax coloring
(setq org-src-fontify-natively t)

;;------------------------------------------------------------------------------
;; Completion for Minibuffer Prompts
;;
;;Ido Mode Setup
(progn
  ;; make buffer switch command do suggestions, also for find-file command
  (require 'ido)
  (ido-mode 1)

  ;; show choices vertically
  (if (version< emacs-version "25")
      (progn
        (make-local-variable 'ido-separator)
        (setq ido-separator "\n"))
    (progn
      (make-local-variable 'ido-decorations)
      (setf (nth 2 ido-decorations) "\n")))

  ;; show any name that has the chars you typed
  (setq ido-enable-flex-matching t)
  ;; use current pane for newly opened file
  (setq ido-default-file-method 'selected-window)
  ;; use current pane for newly switched buffer
  (setq ido-default-buffer-method 'selected-window)
  ;; stop ido from suggesting when naming new file
  (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil))

;; Icomplete Mode
(progn
  ;; minibuffer enhanced completion
  (require 'icomplete)
  (icomplete-mode 1)
  ;; show choices vertically
  (setq icomplete-separator "\n")
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-in-buffer t)

  (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions))


;; Open Recently Opened File
;; keep a list of recently opened files
(recentf-mode 1)
;; set F7 to list recently opened file
(global-set-key (kbd "<f7>") 'recentf-open-files)

;; company-mode
;; To use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(package-selected-packages (quote (company))))
;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; )

;; Jump to Previous Position
;; each mark ring keep 6 positions.
(setq mark-ring-max 6)
(setq global-mark-ring-max 6)
;; single key to jump thru marks
(defun mlzmxy-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
version 2016-04-04"
  (interactive)
  (set-mark-command t))

(global-set-key (kbd "M-7") 'pop-global-mark) ; Meta+7
(global-set-key (kbd "M-8") 'mlzmxy-pop-local-mark-ring) ; Meta+8


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
