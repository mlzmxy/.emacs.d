(require 'package)

;; --------------------------------------------------------------------------
;; load the plain emacs settings

(load "~/.emacs.d/lisp/mlzmxy_emacs_settings.el")

;; --------------------------------------------------------------------------
;; load packages

; 
(add-to-list 'load-path
                "~/.emacs.d/snippets/")
   (require 'yasnippet)
   (yas-global-mode 1)

;; --------------------------------------------------------------------------
;; load files

(load "~/.emacs.d/lisp/mlzmxy_emacs_keybinding.el")

(load "~/.emacs.d/lisp/mlzmxy_emacs_misc.el")
