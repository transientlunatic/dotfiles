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

;;;;; Custom set faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro"))))
 '(org-block ((t (:inherit variable-pitch))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight normal :foreground "#f8f8f2" :height 1.5 :font "EB Garamond" :height 2.5))))
 '(org-hide ((t (:foreground nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight normal :foreground "#f8f8f2" :height 1.5 :font "EB Garamond" :height 2.0 :weight bold))))
 '(org-level-2 ((t (:inherit default :weight normal :foreground "#f8f8f2" :height 1.5 :font "EB Garamond" :height 1.8 :slant italic))))
 '(org-level-3 ((t (:inherit default :weight normal :foreground "#f8f8f2" :height 1.5 :font "EB Garamond" :height 1.5 :slant italic))))
 '(org-level-4 ((t (:inherit default :weight normal :foreground "#f8f8f2" :height 1.5 :font "EB Garamond" :height 1.25 :slant italic))))
 '(org-level-5 ((t (:inherit default :weight normal :foreground "#f8f8f2" :height 1.5 :font "EB Garamond"))))
 '(org-level-6 ((t (:inherit default :weight normal :foreground "#f8f8f2" :height 1.5 :font "EB Garamond"))))
 '(org-level-7 ((t (:inherit default :weight normal :foreground "#f8f8f2" :height 1.5 :font "EB Garamond"))))
 '(org-level-8 ((t (:inherit default :weight normal :foreground "#f8f8f2" :height 1.5 :font "EB Garamond"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "dark orange"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.6))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "EB Garamond" :height 180 :weight light :foreground "#DDDDDD")))))


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



;;
;; Load dracula theme
;;
(setq custom-safe-themes t)
(load-theme 'dracula t)

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
(require 'bind-key
)

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
		    (add-hook 'org-mode-hook 'variable-pitch-mode)
		    (setq line-spacing 0.2)
		    :config
		     ;; Custom font faces for orgmode
		     (load "~/.dotfiles/emacs/.emacs.d/orgmode.el")
		     (load "~/.dotfiles/emacs/.emacs.d/orgmode-style.el")
		     (require 'org-crypt)
		     (org-crypt-use-before-save-magic)
		     (setq org-tags-exclude-from-inheritance (quote ("crypt")))
		     (setq org-crypt-key "D9787B13D139D6A2")
		     (setq auto-save-default nil)
		     )



(use-package neotree
  :ensure t
  )


;;
;; Org ref
;;
;; (use-package org-ref
;; 	     :ensure t
;; 	     :after (org)
;; 	     :commands org-ref
;; 	     )



;;
;; Org wiki
;;

(use-package org-wiki
  :ensure f
  :after (org)
  :config
  (progn
    (setq org-wiki-default-read-only t)  
    (setq org-wiki-close-root-switch t)
    (setq org-wiki-location-list '(
				   "~/notes/wiki/science"
				   "~/notes/wiki/dnd"))
    (setq org-wiki-location (car org-wiki-location-list))
    )
  )

;;
;; Org mode bullets
;;

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))

		  )
  )

;; ;;
;; ;; Org-protocol
;; ;;

(use-package org-protocol
  :after (org)
  :init (server-start)
  )

;; ;;
;; ;; Org export (ox)
;; ;;
(use-package ox-twiki
  :ensure t
  :after (org)
  )




;;
;; Spaceline
;;
;(use-package spaceline-config
;  :init (spaceline-emacs-theme)
;  )



;; ;; Org publication
(defun my-org-publish-buffer ()
  (interactive)
  (save-buffer)
  (save-excursion (org-publish-current-file))
  (let* ((proj (org-publish-get-project-from-filename buffer-file-name))
         (proj-plist (cdr proj))
         (rel (file-relative-name buffer-file-name
                                  (plist-get proj-plist :base-directory)))
         (dest (plist-get proj-plist :publishing-directory)))
    (browse-url (concat "file://"
                        (file-name-as-directory (expand-file-name dest))
                        (file-name-sans-extension rel)
                        ".html"))))



;; Org table aggregate

;; (use-package orgtbl-aggregate
;;   :ensure f
;;   :after (org)
;;   )


;;
;; org-mode latex export (ox-latex)
;;

(use-package ox-latex
  :config
  (progn
    (setq org-latex-default-packages-alist (cons '("mathletters" "ucs" nil) org-latex-default-packages-alist))

    (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
    
    (add-to-list 'org-latex-classes
		 '("momento" "\\documentclass{momento}"
		   ("\\chapter{%s}" . "\\chapter*{%s}")
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    
    (add-to-list 'org-export-exclude-tags
		 '("embargoed")
		 )
    ;; export property drawers in a nicer way
    (defun org-latex-property-drawer (property-drawer contents info)
      ;; Transcode a PROPERTY-DRAWER element from Org to LaTeX.
      ;;CONTENTS holds the contents of the drawer.  INFO is a plist
      ;;holding contextual information.
      (and (org-string-nw-p contents)
	   (format "{\\tiny \\hrule \\pgfplotstabletypeset[col sep=colon,begin table={\\newline\\begin{tabularx}{8.5cm}{lX}}, end table={\\end{tabularx}}, columns/Key/.style={string type}, columns/Value/.style={string type}]{Key:Value\n%s}\\hrule}" contents))
      )
    (setq org-latex-default-class "momento")
    ;; Set-up the compilation process for a latex file. Should probably change this to mklatex
    (setq org-latex-pdf-process
	  '("pdflatex -interaction nonstopmode -output-directory %o %f"
	    "bibtex %b"
	    "pdflatex -interaction nonstopmode -output-directory %o %f"
	    "pdflatex -interaction nonstopmode -output-directory %o %f"))
    (setq org-latex-create-formula-image-program 'imagemagick)
    (setq org-latex-packages-alist
	  (quote (("" "color" t)
		  ("" "minted" t)
		  ("" "parskip" t)
		  ("" "pgfplots" t)
		  ("" "tikz" t))))
    )
  )


;; ;;
;; ;; org publication
;; ;;

(use-package org-jekyll
  :after (org)
  :ensure t
  )

(use-package ox-twbs
  :after (org)
  :ensure t
  )

(use-package ox-publish
  :config
  (progn
    (setq org-publish-project-alist
	  '(
	    ("org-gravwaves"
	     :base-directory "~/notes/gravitational-waves"
	     :publishing-directory "~/www/notebook/gravitational-waves"
	     :recursive t
	     :publishing-function org-twbs-publish-to-html ;;org-twbs-publish-to-html
	     :with-sub-superscript nil
	     )
	    
	    ("org-static"
	     :base-directory "~/notes/"
	     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	     :publishing-directory "~/www/"
	     :recursive t
	     :publishing-function org-publish-attachment
	     )
	    
	    ("papers-library"
	     :base-directory "~/notes/papers"
	     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	     :publishing-directory "~/www/papers"
	     :recursive t
	     :publishing-function org-publish-attachment
	     )
	    
	    ("bibliography"
	     :base-directory "~/notes/bibliography"
	     :publishing-directory "~/www/bibliography"
	     :publishing-function org-twbs-publish-to-html
	     :with-sub-superscript nil
	     :auto-sitemap t
	     )
	    
	    ("journal"
	     :base-directory "~/notes/research"
	     :publishing-directory "~/www/journal"
	     :publishing-function org-twbs-publish-to-html
	     :with-sub-superscript nil
	     :auto-sitemap t
	     )
	    
	    ("projects"
	     :base-directory "~/notes/projects"
	     :publishing-directory "~/www/projects"
	     :publishing-function org-twbs-publish-to-html
	     :with-sub-superscript nil
	     :auto-sitemap t
	     )
	    )
      )
    )
  )

(use-package magit
  :ensure t
  :commands magit-status
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  )

;; (setq package-check-signature nil)
;; (use-package org-gcal
;;   :ensure t
;;   :config
;;   (require 'secrets "~/.emacs.d/secrets.el.gpg")
;;   (setq org-gcal-file-alist '(("pulsar.co.nr@gmail.com" . "~/notes/cals/google.org")
;; 			      ("ct240d39oc9kq21cq3bn70jii8@group.calendar.google.com" . "~/notes/cals/international.org")
;; 			      ("2s2ausqn4j7g6bjhoth8vnrj0c@group.calendar.google.com". "~/notes/cals/calls.org")
;; 			      ("5vskop5jidv3vpo10gucv611s4eeau5f@import.calendar.google.com" . "~/notes/cals/pro14.org")
;; 			      ("q1v1coujord5pk00mdtdu6leuajqdclo@import.calendar.google.com" . "~/notes/cals/eprc.org")
;; 			      ("35g1iaek8hramundse382il848@group.calendar.google.com" . "~/notes/cals/hyndlandrfc.org")))
;; 	(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;; 	(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
;; 	)


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


;; (clear-abbrev-table global-abbrev-table)

;; (define-abbrev-table 'global-abbrev-table
;;   '(
;;     ("md" 0 "—" )
;;     ("bu" 0 "•" )
;;     ("<-" 0 "←" )
;;     ("->" 0 "→" )
;;     ("uutf8" 0 "-*- coding: utf-8 -*-" )
;;     ))

;; (set-default 'abbrev-mode t)

;; (setq save-abbrevs nil)

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

;;; Autocompleation utilities
(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))
(use-package company-jedi
  :ensure t)

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

(setq package-check-signature nil)
(unless (and (eq package-check-signature 'allow-unsigned)
             (eq (epg-signature-status sig) 'no-pubkey))
  (setq had-fatal-error t))
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators (quote (";" "	")))
 '(line-number-mode nil)
 '(org-agenda-files
   (quote
    ("~/thesis/chapters/sources/sources.org" "/home/daniel/notes/research/sitemap.org" "/home/daniel/notes/projects/Burst_MDC.org" "/home/daniel/notes/projects/O2BurstMDC.org" "/home/daniel/notes/projects/acreroad.org" "/home/daniel/notes/projects/armadillo.org" "/home/daniel/notes/projects/grbeaming.org" "/home/daniel/notes/projects/heron.org" "/home/daniel/notes/projects/minke.org" "/home/daniel/notes/projects/outreach.org" "/home/daniel/notes/projects/pydv.org" "/home/daniel/notes/projects/reddit-ama.org" "/home/daniel/notes/projects/salamander.org" "/home/daniel/notes/projects/sitemap.org" "/home/daniel/notes/projects/thesis.org" "~/notes/cals/google.org" "~/notes/cals/international.org" "~/notes/cals/pro14.org" "~/notes/cals/eprc.org" "~/notes/cals/hyndlandrfc.org" "/home/daniel/notes/research/2019-09-23.org.gpg" "/home/daniel/notes/research/2019-09-10.org.gpg")))
 '(org-journal-carryover-items "TODO=\"TODO\"|TODO=\"TODAY\"|TODO=\"MERGE\"" t)
 '(org-journal-dir "~/notes/research/" t)
 '(org-journal-enable-agenda-integration t t)
 '(org-journal-enable-encryption t t)
 '(org-journal-encrypt-journal t t)
 '(org-journal-file-format "%Y-%m-%d.org" t)
 '(org-journal-file-type (quote daily) t)
 '(package-selected-packages
   (quote
    (elpy mmm-mode company-jedi company company-mode neotree ox-rst workgroups epa-file yaml-mode wanderlust virtualenv use-package-el-get tide spaceline-all-the-icons rainbow-mode pass pandoc-mode ox-twiki ox-twbs ox-latex-chinese org2jekyll org-wiki org-time-budgets org-sync org-ref org-protocol-jekyll org-journal org-jekyll org-gcal org-edit-latex org-easy-img-insert org-download org-dashboard org-caldav org-bullets org-ac ob-ipython ob-browser multi-web-mode markdown-edit-indirect magit ledger-mode latex-extra json-mode jedi helm-bibtexkey gitlab gist ein dracula-theme dockerfile-mode diminish csv-mode cdlatex auto-virtualenvwrapper))))
