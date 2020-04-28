;;
;; Org crypt
;;

(use-package org-crypt
  :ensure f
  :after (org)
  :config
  (org-crypt-use-before-save-magic)
  :custom
  (auto-save-default nil)
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
  :custom
  (org-journal-encrypt-journal t)
  ; (org-journal-enable-encryption t)
  (org-journal-dir "~/notes/research/")
  (org-journal-file-type 'daily)
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-enable-agenda-integration t)
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"TODAY\"|TODO=\"MERGE\"|TODO=\"WAITING\"")
  (org-journal-skip-carryover-drawers (list "LOGBOOK"))
  (org-journal-find-file 'find-file)
  ;;
  
  :config
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))
  
  ;; (add-to-list 'org-capture-templates '(
  ;;                                       ("j" "Journal entry" entry (function org-journal-find-location)
  ;;                                        "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))
  )



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

;; (use-package org-jekyll
;;   :after (org)
;;   :ensure t
;;   )

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
;;     (setq org-wiki-default-read-only t)  
;;     (setq org-wiki-close-root-switch t)
;;     (setq org-wiki-location-list '(
;; 				   "~/notes/wiki/science"
;; 				   "~/notes/wiki/dnd"))
;;     (setq org-wiki-location (car org-wiki-location-list))
;;     )
;;   )
