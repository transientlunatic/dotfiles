
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
  ; (org-journal-enable-encryption t)
  (org-journal-dir "~/notes/journal/")
  (org-journal-file-type 'daily)
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-enable-agenda-integration t)
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"DOING\"|TODO=\"TODAY\"|TODO=\"MERGE\"|TODO=\"WAITING\"")
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
