;;
;; load libraries
;;
;(load-library "find-lisp")

;;
;; Language and display customisations
;;

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq vc-follow-symlinks t)
;; Set up the line-wrapping
(global-visual-line-mode t)

;;
;; Load the package repositories
;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
	     '("org" . "http://orgmode.org/elpa/")
	     )
(add-to-list 'package-archives '("milkbox" . "http://melpa.milkbox.net/packages/") t)
(setq package-archive-priorities '(("org" . 4)
				   ("milkbox" . 1)
                                   ("melpa" . 2)))
(package-initialize)




;; Inhibit the startup screen
(setq inhibit-startup-screen t)


;; 
;; Bootstrap `use-package'
;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(require 'use-package-ensure)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))



;;
;; Load dracula theme
;;
(use-package dracula-theme
	     :ensure t
	     :init
	     (setq custom-safe-themes t)
	     (load-theme 'dracula t))

;;
;; PGP-style encyryption
;;
;; (use-package epa-file
;;  :ensure t
;;  :init (epa-file-enable)
;;  )
  
(use-package workgroups
  :ensure t
  :diminish workgroups-mode
  :config
  (setq wg-prefix-key (kbd "C-c w"))
  (workgroups-mode 1)
  ;(wg-load "~/.dotfiles/emacs/.emacs.d/workgroups")
  )

;;
;; org-mode
;;

(use-package org
	     :ensure t
	     :mode ("\\.org\\'" . org-mode)
	     :bind (("C-c l" . org-store-link)
		    ("C-c c" . org-capture)
		    ("C-c a" . org-agenda)
		    ("C-c b" . org-iswitchb)
		    ("C-c C-w" . org-refile)
		    ("C-c j" . org-clock-goto)
		    ("C-c C-x C-o" . org-clock-out)
		    ("C-c C-x C-b" . org-bibtex-yank)
		    )
	     :init ;(add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))
	     (add-hook 'org-mode-hook 'visual-line-mode)
	     (add-hook 'org-mode-hook 'flyspell-mode)
					;(add-hook 'org-mode-hook 'variable-pitch-mode)
					;(setq line-spacing 0.2)
	     :config
	     ;; Custom font faces for orgmode
	     (load "~/.dotfiles/emacs/.emacs.d/orgmode.el")
					;(load "~/.dotfiles/emacs/.emacs.d/orgmode-style.el")
	     )
(load "~/.dotfiles/emacs/.emacs.d/orgmode-extras.el")



;;; File explorer

(use-package neotree
  :ensure t
  )

(use-package magit
  :ensure t
  :commands magit-status
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  )


;;; Web editing

(use-package multi-web-mode
  :ensure f
  :config
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags 
	'((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
	  (js-mode  "<script[^>]*>" "</script>")
	  (css-mode "<style[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  (multi-web-global-mode 1))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  ;; :hook ((typescript-mode . tide-setup)
  ;;        (typescript-mode . tide-hl-identifier-mode)
  ;;        (before-save . tide-format-before-save))
)


(use-package ledger-mode
  :ensure f
  :config
  (setq ledger-use-iso-dates t)
  (setq ledger-default-date-format "%Y-%m-%d")
  )

;;; Project management

(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/projects"))
  )


;;; Autocompleation utilities
(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

;; Python things
(load "~/.dotfiles/emacs/.emacs.d/python.el")


(setq package-check-signature nil)
(unless (and (eq package-check-signature 'allow-unsigned)
             (eq (epg-signature-status sig) 'no-pubkey))
  (setq had-fatal-error t))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(csv-separators (quote (";" "	")))
;;  '(line-number-mode nil)
;;  '(org-agenda-files
;;    (quote
;;     ("~/thesis/chapters/sources/sources.org" "/home/daniel/notes/research/sitemap.org" "/home/daniel/notes/projects/reddit-ama.org" "~/notes/cals/google.org" "~/notes/cals/international.org" "~/notes/cals/pro14.org" "~/notes/cals/eprc.org" "~/notes/cals/hyndlandrfc.org" "/home/daniel/notes/research/2020-04-20.org.gpg")))
;;  '(org-journal-carryover-items "TODO=\"TODO\"|TODO=\"TODAY\"|TODO=\"MERGE\"")
;;  '(org-journal-dir "~/notes/research/")
;;  '(org-journal-enable-agenda-integration t)
;;  '(org-journal-enable-encryption t)
;;  '(org-journal-encrypt-journal t)
;;  '(org-journal-file-format "%Y-%m-%d.org")
;;  '(org-journal-file-type (quote daily))
;;  '(package-selected-packages
;;    (quote
;;     (ox-tufte ox-rst workgroups epa-file yaml-mode wanderlust virtualenv use-package-el-get tide spaceline-all-the-icons rainbow-mode pass pandoc-mode ox-twiki ox-twbs ox-latex-chinese org2jekyll org-wiki org-time-budgets org-sync org-ref org-protocol-jekyll org-journal org-jekyll org-gcal org-edit-latex org-easy-img-insert org-download org-dashboard org-caldav org-bullets org-ac ob-ipython ob-browser multi-web-mode markdown-edit-indirect magit ledger-mode latex-extra json-mode jedi helm-bibtexkey gitlab gist ein dracula-theme dockerfile-mode diminish csv-mode cdlatex auto-virtualenvwrapper))))

;; Configure flymake for Python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; Set as a minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/notes/research/2020-04-27.org.gpg" "/home/daniel/notes/projects/current.org" "/home/daniel/notes/projects/reddit-ama.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
