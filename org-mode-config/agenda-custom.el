;; Custom agenda commands to quickly view lists of relevent data
(setq org-agenda-custom-commands
      '(
        ;; Automatically show table view in agenda mode ordered by date
	("1" . "Custom agenda commands: Sorted tables")

	;; For all TASKS; requires the properties described in org-agenda-overriding-columns-format
	("1t" agenda "Tasks TODO agenda table (active only)"
	 (
	  (org-agenda-skip-function
	   '(org-agenda-skip-entry-if 'nottodo '("TODO" "WAITING" "CANCELLED" "DELEGATED" "DONE")))
	  ;; (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%75ITEM %36ID %100Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )

	;; For all POSSESSIONS; requires the properties described in org-agenda-overriding-columns-format
	("1p" agenda "Possessions TODO agenda table (active and inactive)"
	 (
	  (org-agenda-skip-function
	   '(org-agenda-skip-entry-if 'nottodo '("PURCHASE" "TRANSIT" "SELL" "LOANED" "OWN" "GIFTED" "SOLD" "DISCARDED")))
	  (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%50ITEM %10Cost %10Paid %20Merchant %20Method %20Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )

	;; For all MULTIMEDIA; requires the properties described in org-agenda-overriding-columns-format
	("1m" agenda "Multimedia TODO agenda table (active and inactive)"
	 (
	  (org-agenda-skip-function
	   '(org-agenda-skip-entry-if 'nottodo '("CONSUME" "SUBSCRIBE" "SHARE" "REFERENCE")))
	  (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%11ITEM %10Creator %50Created %10Source %20Link %16Date %20Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )

	;; For all EVENTS; requires the properties described in org-agenda-overriding-columns-format
	("1e" agenda "Events TODO agenda table (active)"
	 (
	  (org-agenda-skip-function
	   '(org-agenda-skip-entry-if 'nottodo '("VISIT" "PLANNED" "MEETING" "VISITED")))
	  ;; (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%50ITEM %50Attend %20Location %20Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )
	
	;; For all FINANCES; requires the :fin: tag and the properties described in org-agenda-overriding-columns-format
	("1f" agenda "Finance tag agenda table (active)"
	 (
	  (org-agenda-filter-preset '("+fin")) ;; instead of org-agenda-tag-filter-preset and org-agenda-filter-by-tag
	  ;; (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%50ITEM %10Cost %10Paid %20Merchant %20Method %20Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )

	;; For all NOTES; requires the :note: tag and the properties described in org-agenda-overriding-columns-format
	("1n" agenda "Note tag agenda table (inactive)"
	 (
	  (org-agenda-filter-preset '("+note")) ;; instead of org-agenda-tag-filter-preset and org-agenda-filter-by-tag
	  (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%75ITEM %36ID %75Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )

	;; Automatically show table view in agenda mode ordered as found
	("2" . "Custom global TODO list commands: Unsorted tables")

	;; For all TASKS; requires the properties described in org-agenda-overriding-columns-format
	("2t" "Tasks global TODO list table (active only)" todo "TODO|WAITING|CANCELLED|DELEGATED|DONE"
	 (
	  ;; (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%75ITEM %36ID %100Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )

	;; For all POSSESSIONS; requires the properties described in org-agenda-overriding-columns-format
	("2p" "Possessions global TODO list table (active and inactive)" todo "PURCHASE|TRANSIT|SELL|LOANED|OWN|GIFTED|SOLD|DISCARDED"
	 (
	  (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%50ITEM %10Cost %10Paid %20Merchant %20Method %20Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )

	;; For all MULTIMEDIA; requires the properties described in org-agenda-overriding-columns-format
	("2m" "Multimedia global TODO list table (active and inactive)" todo "CONSUME|SUBSCRIBE|SHARE|REFERENCE"
	 (
	  (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%11ITEM %10Creator %50Created %10Source %20Link %16Date %20Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )

	;; For all EVENTS; requires the properties described in org-agenda-overriding-columns-format
	("2e" "Events global TODO List table (active)" todo "VISIT|PLANNED|MEETING|VISITED"
	 (
	  ;; (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%50ITEM %50Attend %20Location %20Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )

	;; For all finances; requires them to have the :fin: tag and the properties described in org-agenda-overriding-columns-format
	("2f" "Finance global TODO list table (active)" tags "fin"
	 (
	  ;; (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%50ITEM %10Cost %10Paid %20Merchant %20Method %20Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )

	;; For all NOTES; requires the :note: tag and the properties described in org-agenda-overriding-columns-format
	("2n" "Note tag global TODO list table (inactive)" tags "note"
	 (
	  (org-agenda-include-inactive-timestamps 't)
	  (org-agenda-overriding-columns-format "%75ITEM %36ID %75Note")
	  (org-agenda-view-columns-initially t)
	  )
	 )
	)
      )
