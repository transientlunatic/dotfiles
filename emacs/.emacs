;;
;; load libraries
;;
(load-library "find-lisp")


;;
;; Language and display customisations
;;

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)


;;;;; Custom set faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 129 :width normal :family "Hack"))))
)




;;
;; Custom-set variables
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 )

;;
;; Load the package repositories
;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") 
	     '("org" . "http://orgmode.org/elpa/") )
(setq package-archive-priorities '(("org" . 4)
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
		    )
	     :init (add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))
	     :config
	     (progn
	       (setq org-directory "~/notes")
	       (setq org-default-notes-file (concat org-directory "/captures.org"))
	       (setq org-agenda-files
		     (find-lisp-find-files "~/notes" "\.org$"))
	       ;; Org-mode workflow states
	       (setq org-todo-keywords
		     '((sequence "TODO" "TODAY" "TEST" "REPORT" "MERGE" "|" "DONE" "DELEGATED")
		       (sequence "TO READ" "MAKE NOTES" "REVIEW" "|" "READ")
		       )
		     )
	       ;; Set Org TODO Faces
	       (setq org-todo-keyword-faces
		     '(("TODO" . (:weight bold :background "firebrick" :foreground "white"))
		       ("TODAY" . (:weight bold :background "IndianRed2" :foreground "white"))
		       ("TEST" . (:weight bold :background "orange" :foreground "black"))
		       ("REPORT" . (:weight bold :background "thistle" :foreground "black"))
		       ("STARTED" . "yellow")
		       ("TO READ" . (:weight bold :background "DarkOrchid" :foreground "white"))
		       ("MAKE NOTES" . (:weight bold :background "thistle" :foreground "black"))
		       ("HABIT" . (:weight bold :background "DeepPink" :foreground "white"))
		       ("CANCELED" . (:foreground "light grey" :weight bold)))
		     )
	       ;; Set Org TAG Faces
	       (setq org-tag-faces
		     '(("minke" . (:background "LightSlateBlue" :foreground "white"))
		       ;; 	("embargoed" . (:background "red" :foreground "white"))
		       ))
	       
	       ;; Capture templates
	       (setq org-capture-templates
		     '(
		       ("w" "Web site" entry (file+olp "~/notes/capture.org") "* %c :website:\n%U %?%:initial")
		       
		       )
		     )
	       ;; Org HTML
	       (setq org-publish-project-alist
		     '(("org-notes"
			:base-directory "~/notes/"
			:publishing-directory "~/www/org/"
			:publishing-function org-twbs-publish-to-html
			:with-sub-superscript nil
			)))
	       (custom-theme-set-faces 'user
				       `(org-level-8 ((t (,@headline ,@variable-tuple))))
				       `(org-level-7 ((t (,@headline ,@variable-tuple))))
				       `(org-level-6 ((t (,@headline ,@variable-tuple))))
				       `(org-level-5 ((t (,@headline ,@variable-tuple))))
				       `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
				       `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
				       `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
				       `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
				       `(org-todo ((t (,@headline ,@variable-tuple :height 1.1))))
				       `(org-document-title ((t (,@headline ,@variable-tuple :height 2.5  :underline nil)))))

	       ;; Org babel
	       
	       (org-babel-do-load-languages
		'org-babel-load-languages
		'((emacs-lisp . t)
		  (python . t)
		  (ditaa . t)
		  (dot . t)
		  (gnuplot . t)
		  (org . t)
		  (latex . t)))

	       ;; Custom font faces for orgmode
	       (let* ((variable-tuple (cond ((x-list-fonts "Raleway") '(:font "Raleway"))
					    ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
					    ((x-list-fonts "Verdana")         '(:font "Verdana"))
					    ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
					    (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
		      (base-font-color     (face-foreground 'default nil 'default))
		      (headline           `(:inherit default :foreground ,base-font-color)))
		 
		 
		 )

	       )

	     
	     )

;;
;; Org journal
;;

(use-package org-journal
	     :ensure t
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
(use-package org-ref
	     :ensure t
	     :after (org)
	     :commands org-ref
	     )



;;
;; Org wiki
;;

(use-package org-wiki
  :ensure t
  :after (org)
  :config
  (progn 
    (setq org-wiki-location "~/notes/wiki")
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

;;
;; Org-protocol
;;

(use-package org-protocol
  :ensure t
  :after (org)
  :init (server-start)
  )

;;
;; Org export (ox)
;;
(use-package ox-twiki
  :ensure t
  :after (org)
  )




;;
;; Spaceline
;;
(use-package spaceline-config
  :ensure t
  :init (spaceline-emacs-theme)
  )



;; Org publication
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






;;
;; org-mode latex export (ox-latex)
;;

(use-package ox-latex
  :ensure t
  :config
  (progn
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


;;
;; org publication
;;

(use-package org-jekyll
  :after (org)
  :ensure t
  )

(use-package ox-publish
  :after (org-jekyll)
  :ensure t
  :config
  (progn
    (setq org-publish-project-alist
      '(("org-gravwaves"
         :base-directory "~/notes/gravitational-waves"
         :publishing-directory "~/website/notebook/gravitational-waves"
	 :recursive t
         :publishing-function org-twbs-publish-to-html ;;org-twbs-publish-to-html
         :with-sub-superscript nil
         )
      ("org-static"
       :base-directory "~/org/"
       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
       :publishing-directory "~/public_html/"
       :recursive t
       :publishing-function org-publish-attachment
       ))
      )
    )
  )

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






