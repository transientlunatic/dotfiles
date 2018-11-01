;;
;; load libraries
;;
;(load-library "find-lisp")
(add-to-list 'load-path "~/.emacs.d/")

;;
;; Language and display customisations
;;

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

'(vc-follow-symlinks t)

;;;;; Custom set faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :family "Source Code Pro"))))
 '(org-document-title ((t (:inherit default :weight normal :height 1.5 :font "Raleway" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight normal :height 1.5 :font "Raleway" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight normal :height 1.5 :font "Raleway" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight normal :height 1.5 :font "Raleway" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight normal :height 1.5 :font "Raleway" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight normal :height 1.5 :font "Raleway"))))
 '(org-level-6 ((t (:inherit default :weight normal :height 1.5 :font "Raleway"))))
 '(org-level-7 ((t (:inherit default :weight normal :height 1.5 :font "Raleway"))))
 '(org-level-8 ((t (:inherit default :weight normal :height 1.5 :font "Raleway")))))

;; Custom font faces for orgmode
(let* ((variable-tuple (cond ((x-list-fonts "Raleway") '(:font "Raleway"))
			     ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight normal :foreground ,base-font-color :height 1.5)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))




;;
;; Custom-set variables
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   (quote
    ("aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" default)))
 '(frame-background-mode (quote dark))
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("/home/daniel/notes/diary/2018.org" "/home/daniel/notes/projects/Burst_MDC.org" "/home/daniel/notes/projects/O2BurstMDC.org" "/home/daniel/notes/projects/acreroad.org" "/home/daniel/notes/projects/armadillo.org" "/home/daniel/notes/projects/damselfly.org" "/home/daniel/notes/projects/grbeaming.org" "/home/daniel/notes/projects/heron.org" "/home/daniel/notes/projects/minke.org" "/home/daniel/notes/projects/outreach.org" "/home/daniel/notes/projects/pydv.org" "/home/daniel/notes/projects/reddit-ama.org" "/home/daniel/notes/projects/salamander.org" "/home/daniel/notes/projects/sitemap.org" "/home/daniel/notes/projects/thesis.org" "~/notes/cals/google.org" "~/notes/cals/international.org" "~/notes/cals/pro14.org" "~/notes/cals/eprc.org" "~/notes/cals/hyndlandrfc.org")))
 '(package-selected-packages
   (quote
    (tide yaml-mode wanderlust virtualenv use-package-el-get spaceline-all-the-icons rainbow-mode pass pandoc-mode ox-twiki ox-twbs ox-latex-chinese org2jekyll org-wiki org-time-budgets org-sync org-ref org-protocol-jekyll org-journal org-jekyll org-gcal org-edit-latex org-easy-img-insert org-download org-dashboard org-caldav org-bullets org-ac ob-browser multi-web-mode markdown-edit-indirect magit ledger-mode latex-extra json-mode jedi helm-bibtexkey gitlab gist ein dockerfile-mode diminish cdlatex auto-virtualenvwrapper)))
 '(safe-local-variable-values (quote ((org-emphasis-alist)))))

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
;(use-package epa-file
;  :ensure t
;  :init (epa-file-enable)
;  )
  


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
	     :init (add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))
	     :config
	     (progn
	       (setq org-directory "~/notes")
	       (setq project-directory (concat org-directory "/projects"))

	       (setq org-startup-with-inline-images t)
	       (setq org-image-actual-width nil)
	       
	       (setq org-default-notes-file (concat org-directory "/captures.org"))
	       (setq org-agenda-files (list
				       (concat org-directory "/diary/")
				       (concat org-directory "/projects/")
				       (concat org-directory "/cals/google.org")
				       (concat org-directory "/cals/international.org")
				       (concat org-directory "/cals/pro14.org")
				       (concat org-directory "/cals/eprc.org")
				       (concat org-directory "/cals/hyndlandrfc.org") ))
	       ;; Org-mode workflow states
	       (setq org-todo-keywords
		     '((sequence "TODO" "TODAY" "TEST" "REPORT" "MERGE" "|" "DONE" "DELEGATED")
		       (sequence "TO READ" "MAKE NOTES" "REVIEW" "|" "READ")
		       )
		     )
	       ;; Set Org TODO Faces
	       (setq org-todo-keyword-faces
		     '(("TODO" . (:weight bold  :foreground "firebrick"))
		       ("TODAY" . (:weight bold :foreground "IndianRed2" :background "white"))
		       ("TEST" . (:weight bold :background "orange" :foreground "black"))
		       ("REPORT" . (:weight bold :background "thistle" :foreground "black"))
		       ("STARTED" . "yellow")
		       ("TO READ" . (:weight bold :background "DarkOrchid" :foreground "white"))
		       ("MAKE NOTES" . (:weight bold :background "thistle" :foreground "black"))
		       ("HABIT" . (:weight bold :background "DeepPink" :foreground "white"))
		       ("CANCELED" . (:foreground "light grey" :weight bold)))
		     )


	       (org-link-set-parameters
		"dcc"
		:follow (lambda (handle)(browse-url (concat "https://dcc.ligo.org/LIGO-" handle)))
		)
		

	       
	       	       
	       (font-lock-add-keywords            ; A bit silly but my headers are now
		'org-mode `(("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
			     (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚑")
				       nil)))
			    ("^\\*+ \\(TODAY\\) "
			     (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚐")
				       nil)))
			    ("^\\*+ \\(TO READ\\) "
			     (1 (progn (compose-region (match-beginning 1) (match-end 1) "📖")
				       nil)))
			    ("^\\*+ \\(CANCELED\\) "
			     (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘")
				       nil)))
			    ("^\\*+ \\(GOAL\\) "
			     (1 (progn (compose-region (match-beginning 1) (match-end 1) "★")
				       nil)))
			    ("^\\*+ \\(DONE\\) "
			     (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔")
				       nil)))))


	       
	       ;; Set Org TAG Faces
	       (setq org-tag-faces
		     '( ("minke" . (:background "LightSlateBlue" :foreground "white"))
			("telecon" . (:background "Blue" :foreground "white"))
			("embargoed" . (:background "red" :foreground "white"))
		       ))
	       
	       ;; Capture templates
	       (setq org-capture-templates
		     '(
		       
		       ("g" "Glossary" entry (file+olp (concat org-directory "/glossary.org") "Glossary")
			"* %^{Term}\n :PROPERTIES:\n :abbreviation: %^{tag}\n :END:\n %?\n" :kill-buffer t)
		       
		       ("w" "Web site" entry (file+olp (concat org-directory "/capture.org") "Website" )
			"* %c :website:\n%U %?%:initial" :kill-buffer t)
		       
		       )
		     )
	       ;; Org HTML
	       (setq org-publish-project-alist
		     '(("org-notes"
			:base-directory "~/notes/"
			:publishing-directory "~/www/org/"
			:publishing-function org-twbs-publish-to-html
			:with-sub-superscript nil
			)

		       )
		     )
		 
	       ;; Org smart-quotes
	       (setq org-export-with-smart-quotes t)

	       ;; Org export css
	       (setq org-export-htmlize-output-type 'css)
	       
	       ;; Org babel
	       
	       (org-babel-do-load-languages
		'org-babel-load-languages
		'((emacs-lisp . t)
		  (python . t)
		  ;(ipython . t)
		  (ditaa . t)
		  (dot . t)
		  (gnuplot . t)
		  (org . t)
		  (latex . t)))


	       )

	     
	     )

;;
;; Org journal
;;

(use-package org-journal
  :ensure t
  :bind (("C-c C-#" . org-journal-new-entry)
	 )
  :commands org-journal-mode
  :after (org)
  :config
  (progn
    (setq org-journal-dir "~/notes/research/"
	  org-journal-file-format "%Y-%m-%d.org")
    )
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

;; (use-package org-wiki
;;   :ensure f
;;   :after (org)
;;   :config
;;   (progn 
;;     (setq org-wiki-location "~/notes/wiki")
;;     )
;;   )

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

(use-package orgtbl-aggregate
  :ensure t
  :after (org)
  )


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

(setq package-check-signature nil)

(use-package org-gcal
  :ensure t
  :config
  (require 'secrets "secrets.el.gpg")
  (setq org-gcal-file-alist '(("pulsar.co.nr@gmail.com" . "~/notes/cals/google.org")
			      ("ct240d39oc9kq21cq3bn70jii8@group.calendar.google.com" . "~/notes/cals/international.org")
			      ("2s2ausqn4j7g6bjhoth8vnrj0c@group.calendar.google.com". "~/notes/cals/calls.org")
			      ("5vskop5jidv3vpo10gucv611s4eeau5f@import.calendar.google.com" . "~/notes/cals/pro14.org")
			      ("q1v1coujord5pk00mdtdu6leuajqdclo@import.calendar.google.com" . "~/notes/cals/eprc.org")
			      ("35g1iaek8hramundse382il848@group.calendar.google.com" . "~/notes/cals/hyndlandrfc.org")))
	(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
	(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
	)


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
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
)
