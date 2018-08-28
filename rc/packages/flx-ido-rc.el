(defun mk-anti-ido-advice (func &rest args)
  "Temporarily disable IDO and call function FUNC with arguments ARGS."
  (interactive)
  (let ((read-file-name-function #'read-file-name-default))
    (if (called-interactively-p 'any)
        (call-interactively func)
      (apply func args))))
(defun mk-disable-ido (command)
  "Disable IDO when command COMMAND is called."
  (advice-add command :around #'mk-anti-ido-advice))

(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(mk-disable-ido 'dired-create-directory)
