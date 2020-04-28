(let* ((variable-tuple (cond ((x-list-fonts "EB Garamond")     '(:font "EB Garamond"))
			     ((x-list-fonts "Lato")            '(:font "Lato"))
			     ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
			     ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
			     ((x-list-fonts "Verdana")         '(:font "Verdana"))
			     ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
			     (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight normal :foreground ,base-font-color :height 1.5)))
)
  ;; (custom-theme-set-faces 'user
  ;; 			  '(variable-pitch ((t (:family "Lato"
  ;; 							:height 170
  ;; 							:weight light
  ;; 							:foreground "#DDDDDD"
  ;; 							))))
  ;; 			  '(org-table ((t (:inherit fixed-pitch :height 0.8 :foreground "dark gray"))))
  ;; 			  `(org-level-8 ((t (,@headline ,@variable-tuple))))
  ;; 			  `(org-level-7 ((t (,@headline ,@variable-tuple))))
  ;; 			  `(org-level-6 ((t (,@headline ,@variable-tuple))))
  ;; 			  `(org-level-5 ((t (,@headline ,@variable-tuple))))
  ;; 			  `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.25)))) ; :slant italic 
  ;; 			  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.5))))
  ;; 			  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.8))))
  ;; 			  `(org-level-1 ((t (,@headline ,@variable-tuple :height 2.0 :weight bold))))
  ;; 			  `(org-block                 ((t (:inherit variable-pitch))))
  ;; 			  `(org-document-info         ((t (:foreground "dark orange"))))
  ;; 			  `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;; 			  `(org-link                  ((t (:foreground "royal blue" :underline t))))
  ;; 			  `(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;; 			  `(org-property-value        ((t (:inherit fixed-pitch))) t)
  ;; 			  `(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;; 			  `(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.6))))
  ;; 			  `(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
  ;; 			  `(org-hide                  ((t (:foreground nil))))
  ;; 			  `(org-indent                ((t (:inherit (org-hide fixed-pitch)))))
  ;; 			  `(org-special-keyword       ((t (:inherit fixed-pitch :height 0.5))))
  ;; 			  `(org-document-title ((t (,@headline ,@variable-tuple :height 2.5))))))

(setq 
      org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
      org-ellipsis "  " ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      ;; show actually italicized text instead of /italicized text/
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)
