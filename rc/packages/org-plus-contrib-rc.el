(defun my/org-mode-hook ()
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :background nil :box nil))
  (set-face-attribute 'org-checkbox-statistics-todo nil :background nil)
  (set-face-attribute 'org-checkbox-statistics-done nil :background nil)
  (set-face-attribute 'org-todo nil :background nil)
  (set-face-attribute 'org-done nil :background nil))

(add-hook 'org-load-hook #'my/org-mode-hook)
