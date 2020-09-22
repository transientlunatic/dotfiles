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
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("milkbox" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-priorities '(("org" . 4)
				   ("melpa-stable" . 2)
				   ("milkbox" . 1)
                                   ("melpa" . 3)))
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


(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)

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
(load "~/.dotfiles/emacs/.emacs.d/orgjournal.el")



;;; File explorer

(use-package neotree
  :ensure t
  )

(use-package magit
  :ensure t
  :commands magit-status
  :pin melpa-stable
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
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
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
    ("~/notes/meetings/ligo.org" "~/notes/students/weichangfeng.org" "~/notes/meetings/igr-grant.org" "~/notes/meetings/igr-group.org" "/home/daniel/repositories/rugby/notes.org" "/home/daniel/repositories/heron/notes.org" "/home/daniel/.dotfiles/notes.org" "/home/daniel/repositories/asimov/notes.org" "/home/daniel/repositories/ligo/gw-infographic/notes.org" "~/notes/todo.org")))
 '(package-selected-packages
   (quote
    (magit helm-projectile org-projectile json-mode jupyter dockerfile-mode minimap auctex-latexmk auctex yaml-mode markdown-mode workgroups use-package-hydra use-package-ensure-system-package use-package-el-get use-package-chords tide projectile ox-twiki ox-twbs org-protocol-jekyll org-journal-list org-journal org-bullets org-ac neotree multi-web-mode mmm-mode ledger-mode jedi f elpy dracula-theme diminish company-jedi bind-map auto-package-update)))
 '(projectile-mode t nil (projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
