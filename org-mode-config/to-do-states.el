;; Set the TODO item states
;; There are four classes of keywords, TASKS, POSSESSIONS, MULTIMEDIA, and EVENTS
;; They are distinguished by their states and PROPERTIES data
;; There are subsets of the classes that don't have keywords and use tags instead because they don't change state
(setq org-todo-keywords
       '(
	 ;; Sequence for TASKS
	 ;; TODO means it's an item that needs addressing
	 ;; WAITING means it's dependent on something else happening
	 ;; CANCELLED means it's no longer necessary to finish
	 ;; DELEGATED means someone else is doing it
	 ;; DONE means it's complete
	 (sequence "TODO(t)" "WAITING(w@/!)" "|" "CANCELLED(x@/!)" "DELEGATED(e@/!)" "DONE(d@/!)")

	 ;; Sequence for POSSESSIONS
	 ;; PURCHASE means to buy; it's functionally the wishlist
	 ;; TRANSIT means it's in the mail but not here yet
	 ;; SELL means you want to get rid of it, put it up on Craigslist
	 ;; LOANED means someone currently has it
	 ;; UNWANTED is for no longer wanted
	 ;; OWN is for stuff you actually own (may be overlap for reference and own)
	 ;; GIFTED is given to someone as a gift
	 ;; SOLD is sold to someone
	 ;; DISCARDED is for thrown out
	 (sequence "PURCHASE(p)" "TRANSIT(u@/!)" "SELL(k@/!)" "LOANED(n@/!)" "|" "UNWANTED(a@/!)" "OWN(o@/!)" "GIFTED(g@/!)"  "SOLD(c@/!)" "DISCARDED(q@/!)")

	 ;; Sequence for MULTIMEDIA
	 ;; CONSUME means to read (articles, books, quote, etc.), play (games), listen (music), or watch (a series or movie)
	 ;; SUBSCRIBE means to add it to a newsreader or list of some sort
	 ;; SHARE means to share on G+, Facebook, reddit, blog about, etc.
	 ;; IGNORED means not read and no desire to read in the future
	 ;; REFERENCE is for stuff you don't own but want to be available later
	 (sequence "CONSUME(r)" "SUBSCRIBE(b@/!)" "SHARE(s@/!)" "|" "IGNORED(i@/!)" "REFERENCE(f@/!)")

	 ;; Sequence for EVENTS
	 ;; VISIT means that there is something you would physically like to do, no dates associated
	 ;; PLANNED means it's coming up in the future
	 ;; DIDNOTGO means the event was cancelled or I didn't go
	 ;; MEETING means a real time meeting, i.e. at work, or on the phone for something official
	 ;; VISITED means the event took place and is no longer scheduled
	 (sequence "VISIT(v)" "PLANNED(l@/!)" "|" "DIDNOTGO(z@/!)" "MEETING(m)" "VISITED(y@/!)")
	 )
       )

;; Mark the time a TODO item changes state
(setq org-log-done 'time)

;; Add a note when the TODO item changes state
(setq org-log-done 'note)

;; Add state changes to the :LOGBOOK:
(setq org-log-into-drawer t)
