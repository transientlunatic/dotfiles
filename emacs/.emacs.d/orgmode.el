



(progn

  (setq org-crypt-key "mail@daniel-williams.co.uk")
  (setq auto-save-default nil)


  (setq org-directory "~/notes")
  (setq project-directory (concat org-directory "/projects"))

  (setq org-hide-emphasis-markers t) ; Hide markup markers
  
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)
  
  (setq org-default-notes-file (concat org-directory "/captures.org"))
  (setq org-agenda-files (list
					;(concat org-directory "/diary/")
			  (concat org-directory "/projects/")
					;(concat org-directory "/cals/google.org")
					;(concat org-directory "/cals/international.org")
					;(concat org-directory "/cals/pro14.org")
					;(concat org-directory "/cals/eprc.org")
					;(concat org-directory "/cals/hyndlandrfc.org")
			  ))

  ;; Custom org-agenda layout
  (defun air-org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
	  subtree-end
	nil)))
  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.
x
PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
	  (pri-value (* 1000 (- org-lowest-priority priority)))
	  (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
	  subtree-end
	nil)))

  (setq org-agenda-custom-commands
	'(("d" "Daily agenda and all TODOs"
	   (
	    (tags "PRIORITY=\"A\""
		  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		   (org-agenda-overriding-header "High-priority unfinished tasks:")))

	    (tags-todo "TODAY"
		       ((org-agenda-skip-function '(org-agenda-skip-if 'scheduled 'deadline)))
		       (org-agenda-overriding-header "Tasks marked for today:"))
	    
	    (agenda "" ((org-agenda-ndays 1)))

	    (alltodo ""
		     ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
						     (air-org-skip-subtree-if-priority ?A)
						     ;(org-agenda-skip-if 'todo '("WAITING"))
						     (org-agenda-skip-if nil '(scheduled deadline))))
		      (org-agenda-overriding-header "ALL normal priority tasks:")))
	   
	    (tags-todo "WAITING"
		       ((org-agenda-overriding-header "All tasks waiting:"))
		       )
	   
	   )
	   ((org-agenda-compact-blocks t)))))

  
  ;; Org-mode workflow states
  (setq org-todo-keywords
	'((sequence "TODO" "TODAY" "TEST" "REPORT" "MERGE" "WAITING" "|" "DONE" "DELEGATED")
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
	  ("WAITING" . (:weight bold
				:foreground "orange"))
				
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


  (defun dw-cite-link-export (path desc backend)
    (cond
     ((eq 'html backend)
      (format "<a href=\'%s\'>%s</a>" path path))
     ((eq 'latex backend)
      (format "\\cite{%s}" path))
     ;; fall-through case for everything else
     (t
      path)))
  
  (org-link-set-parameters
   "cite"
   :follow (lambda (handle) (open-bibcodes handle))
   :export 'dw-cite-link-export)

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
  


  ;;; TODO List symbols
  
  (font-lock-add-keywords            ; A bit silly but my headers are now
   'org-mode `(("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
		(1 (progn (compose-region (match-beginning 1) (match-end 1) "⚑")
			  nil)))
	       ("^\\*+ \\(TODAY\\) "
		(1 (progn (compose-region (match-beginning 1) (match-end 1) "⚐")
			  nil)))
	       ("^\\*+ \\(WAITING\\) "
		(1 (progn (compose-region (match-beginning 1) (match-end 1) "⌛")
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
	   ("heron" . ( :foreground "white"))
	   ("hypee" . ( :foreground "white" :background "Blue"))
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




;; add new latex document classes

;; (add-to-list 'org-latex-classes
;;              '("tufte-handout"
;;                "\\documentclass{tufte-handout}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
