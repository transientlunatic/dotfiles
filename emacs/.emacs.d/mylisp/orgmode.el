(let* ((variable-tuple (cond ((x-list-fonts "Lato") '(:font "Lato"))
			     ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
			     ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
			     ((x-list-fonts "Verdana")         '(:font "Verdana"))
			     ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
			     (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       
       (headline           `(:inherit default
				      :weight normal
				      :foreground "#bd93f9";,base-font-color
				      :height 1.5)))

  (custom-theme-set-faces 'user
			  '(variable-pitch ((t (:family "Lato" :height 150 :weight light))))
			  `(org-level-8 ((t (,@headline ,@variable-tuple))))
			  `(org-level-7 ((t (,@headline ,@variable-tuple))))
			  `(org-level-6 ((t (,@headline ,@variable-tuple))))
			  `(org-level-5 ((t (,@headline ,@variable-tuple))))
			  `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
			  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
			  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
			  `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
			  '(org-block                 ((t (:inherit fixed-pitch))))
			  '(org-document-info         ((t (:foreground "dark orange"))))
			  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
			  '(org-link                  ((t (:foreground "royal blue" :underline t))))
			  '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
			  '(org-property-value        ((t (:inherit fixed-pitch))) t)
			  '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
			  '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.6))))
			  '(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
			  '(org-indent                ((t (:inherit (org-hide fixed-pitch)))))
			  `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline t))))))

;;; Various appearance tweaks
(setq org-hide-emphasis-markers t) ; Hide markup markers
;; (font-lock-add-keywords 'org-mode  ; Replace bullet points
;;                         '(("^ *\\([-+]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

					; (add-hook 'org-mode-hook 'variable-pitch-mode)


;; function code copied from definition of org-agenda-exit
(add-hook 'org-finalize-agenda-hook
	  (lambda ()
	    (interactive)
	    (org-release-buffers org-agenda-new-buffers)
	    (setq org-agenda-new-buffers nil)))

(progn
  (setq org-directory "~/notes")
  (setq project-directory (concat org-directory "/projects"))

  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)
  
  (setq org-default-notes-file (concat org-directory "/captures.org"))
  (setq org-agenda-files (list
			  (concat "/diary/2019.org")
			  (concat org-directory "/research/")
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
	'(("TODO" . (:weight bold
			     :foreground "firebrick"))
	  
	  ("TODAY" . (:weight bold
			      :foreground "IndianRed2"
			      :background "white"))
	  ("TEST" . (:weight bold
			     :background "orange"
			     :foreground "black"))
	  ("REPORT" . (:weight bold :background "thistle" :foreground "black"))
	  ("STARTED" . "yellow")
	  ("TO READ" . (:weight bold :background "DarkOrchid" :foreground "white"))
	  ("MAKE NOTES" . (:weight bold :background "thistle" :foreground "black"))
	  ("HABIT" . (:weight bold :background "DeepPink" :foreground "white"))
	  ("CANCELED" . (:foreground "light grey" :weight bold)))
	)

	       ;;; Handle tikz previews in org-mode

  (add-to-list 'org-latex-packages-alist
	       '("" "tikz" t))

  (eval-after-load "preview"
    '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

  (setq org-latex-create-formula-image-program 'imagemagick)

  (org-link-set-parameters
   "dcc"
   :follow (lambda (handle)(browse-url (concat "https://dcc.ligo.org/LIGO-" handle)))
   )

	       ;;; end tikz previews


  (defun split-bibcode (handle)
    "Split a link containing multiple bibcodes and return just the first one."
    (interactive "sBibcode: ")
    (setq bibcode-list (split-string handle ","))
    (pop bibcode-list))

  (defun open-bibcodes (handle)
    "Open a bibcode link, potentially with several references."
    (setq bibcode-list (split-string handle ","))
    (dolist (bibcode bibcode-list)
      (browse-url
       (concat "https://ui.adsabs.harvard.edu/#abs/" bibcode "/abstract"))))


					; glossary links
  (setq glossary-file "/home/daniel/thesis/chapters/glossary/glossary.org")
  (defun follow-abb-link (label)
    "Follow an abbreviation link to a label LABEL."
    (interactive "sLabel: ")
    (find-file glossary-file)
    (re-search-forward (format ":ABBREVIATION: %s" (upcase label)) nil t)
    )
  
  (org-link-set-parameters
   "cite"
   :follow (lambda (handle) (open-bibcodes handle))
   )

  (org-link-set-parameters
   "gls"
   :face '(:foreground "orange")
   :follow (lambda (handle)(find-file glossary-file))
   )
  (org-link-set-parameters
   "abb"
   :face '(:foreground "orange")
   :export (lambda (handle desc backend)
	     (cond
	      ((eq 'html backend)
	       (format "<a href=\"glossary.html#%s\">%s</a>" handle (or desc (upcase handle))))))
   :follow (lambda (handle)(follow-abb-link handle))
   )
  (org-link-set-parameters
   "abbr"
   :face '(:foreground "orange" :underline t)
   :export (lambda (handle desc backend)
	     (cond
	      ((eq 'html backend)
	       (format "<a href=\"glossary.html#%s\">%s</a>" handle (or desc (upcase handle))))))
   :follow (lambda (handle)(follow-abb-link handle))
   )
  (org-link-set-parameters
   "abpl"
   :face '(:foreground "orange" :underline t)
   :export (lambda (handle desc backend)
	     (cond
	      ((eq 'html backend)
	       (format "<a href=\"glossary.html#%s\">%s</a>" handle (or desc (upcase handle))))))
   :follow (lambda (handle)(follow-abb-link handle))
   )
  


  ;;; TODO List symbols
  
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
	'( ("minke" . ( :foreground "white"))
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


  ;; Org export backends
  (setq org-export-backends (quote (ascii html icalendar latex md)))
  
  ;; Org babel
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
					;(ipython . t)
     (ditaa . t)
     (ledger . t)
     (dot . t)
     (gnuplot . t)
     (org . t)
     (latex . t)))


  )
