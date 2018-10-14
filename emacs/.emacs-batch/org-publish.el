;;
;; load libraries
;;
(load-library "find-lisp")



;;;;; Custom set faces


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
	       (setq project-directory (concat org-directory "/projects"))

	       (setq org-default-notes-file (concat org-directory "/captures.org"))
	       (setq org-agenda-files
		     (find-lisp-find-files org-directory "\.org$"))

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
		  (ipython . t)
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
  :ensure f
  :after (org)
  :config
  (progn 
    (setq org-wiki-location "~/notes/wiki")
    )
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




