(server-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
    (lambda (package)
      (if (package-installed-p package)
          nil
        (if (y-or-n-p (format "Package %s is missing. Install it? " package))
            (package-install package)
          package)))
    packages))
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(ensure-package-installed
  'material-theme
  'powerline
  'haskell-mode
  'autopair
  'emmet-mode
  'sass-mode
  'linum-relative
  'projectile
  'flx-ido
  'multi-term
  'yaml-mode
  'go-mode
  'clojure-mode
  'markdown-mode
  'elixir-mode
  'arduino-mode
  'org-plus-contrib
  'js2-mode
  'typescript-mode
  'elm-mode
  'coffee-mode
  'kotlin-mode
  'dart-mode
  'php-mode
  'csharp-mode
  'cmake-mode
  'fsharp-mode
  'basic-mode
  'rjsx-mode
  'rainbow-mode
  'rainbow-delimiters
  ;; 'magit
  ;; 'cider
  ;; 'intero
  ;; 'elpy
  ;; 'tide
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOADS / PACKAGES RC:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/rc/packages/js2-mode.el")
(load "~/.emacs.d/rc/flx-ido-rc.el")
(load "~/.emacs.d/rc/projectile-rc.el")
(load "~/.emacs.d/rc/org-plus-contrib-rc.el")
;; (load "~/.emacs.d/rc/linum-relative-rc.el")
(load "~/.emacs.d/rc/emmet-mode-rc.el")
(load "~/.emacs.d/rc/rainbow-mode-rc.el")
(load "~/.emacs.d/rc/rainbow-delimiters-rc.el")
(load "~/.emacs.d/rc/autopair-rc.el")
(load "~/.emacs.d/rc/material-theme-rc.el")
(load "~/.emacs.d/rc/powerline-rc.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOADS / RC:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/rc/no-gui-rc.el")
(load "~/.emacs.d/rc/keybinds-rc.el")
(load "~/.emacs.d/rc/rebinds-rc.el")
(load "~/.emacs.d/rc/variables-rc.el")
(load "~/.emacs.d/rc/fonts-rc.el")
(load "~/.emacs.d/rc/modes-rc.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


