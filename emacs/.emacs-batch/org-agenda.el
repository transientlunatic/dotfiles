(load-library "find-lisp")
(require 'org)
(setq org-directory "~/notes")
(setq project-directory (concat org-directory "/projects"))

(setq org-startup-with-inline-images t)
	       
(setq org-default-notes-file (concat org-directory "/captures.org"))
(setq org-agenda-files
      (find-lisp-find-files org-directory "\.org$"))
