;; Emacs Common Settings

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; full screen
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; frame fullscreen
;(toggle-frame-fullscreen)

;; 去掉工具栏
(tool-bar-mode 0)
;;去掉菜单栏
(menu-bar-mode 0)
;; 去掉滚动栏
(scroll-bar-mode 0)

;; show time
(display-time-mode 1)
(setq display-time-24hr-format t)

;; set default font
(set-frame-font "Noto Sans Mono CJK SC" 14)

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa"
                                 . "https://elpa.emacs-china.org/melpa/"))

;; auto insert closing bracket
(electric-pair-mode 1)

;; turn on bracket match highlight
(progn
  (show-paren-mode 1)

  ;; highlight brackets
  ;(setq show-paren-style 'parenthesis)

  ;; highlight entire expressionq
  ;(setq show-paren-style 'expression)

  ;; highlight brackets if visible, else entire expression
  (setq show-paren-style 'mixed))


;; turn on highlighting curent line
;(global-hl-line-mode 1)

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
;(progn
  ;; minibuffer enhanced completion
;  (require 'icomplete)
;  (icomplete-mode 1)
  ;; show choices vertically
;  (setq icomplete-separator "\n")
;  (setq icomplete-hide-common-prefix nil)
;  (setq icomplete-in-buffer t)

;  (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
;  (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions))


;; Open Recently Opened File
;; keep a list of recently opened files
(recentf-mode 1)
;; set F7 to list recently opened file
(global-set-key (kbd "<f7>") 'recentf-open-files)


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

;; Org Mode
;; Turn on syntax coloring
(setq org-src-fontify-natively t)

;; company-mode
;; To use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; yasnippet settings
(add-to-list 'load-path
                "~/.emacs.d/snippets/")
   (require 'yasnippet)
   (yas-global-mode 1)

;; Helm Settings
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)
;(helm-linum-relative-mode 1)

;;Window-numbering mode
(window-numbering-mode 1)
