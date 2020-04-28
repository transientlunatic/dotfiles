;;; Python utilities
(use-package jedi
  :ensure t
  :init (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package elpy
  :ensure t
  :init (add-hook 'python-mode-hook 'elpy-mode))

(use-package flycheck
  :ensure t
  :init (add-hook 'python-mode-hook 'flycheck-mode))

(use-package company-jedi
  :ensure t)


(use-package mmm-mode
  :ensure t
  :init (setq mmm-global-mode 'maybe)
  (mmm-add-classes
   '((python-rst
    :submode rst-mode
    :front "^ *[ru]?\"\"\"[^\"]*$"
    :back "^ *\"\"\""
    :include-front t
    :include-back t
    :end-not-begin t)))
  (mmm-add-mode-ext-class 'python-mode nil 'python-rst)
  )
