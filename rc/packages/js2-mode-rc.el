(require 'js2-mode)

(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
