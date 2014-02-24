;; Set the TODO item states
;; There are four classes of keywords, TASKS, ITEMS, MEDIA, and EVENTS
;; They are distinguished by their PROPERTIES data
(setq org-todo-keywords
 '(
   ;; Sequence for TASKS
   ;; TODO means it's an item that needs addressing
   ;; WAITING means it's dependent on someone else doing something
   ;; CANCELLED means it's no longer necessary to finish
   ;; DELEGATED means someone else is doing it
   ;; DONE means it's complete
   (sequence "TODO(t)" "WAITING(w@/!)" "|" "CANCELLED(x@/!)" "DELEGATED(e@/!)" "DONE(d@/!)")

   ;; Sequence for ITEMS
   ;; PURCHASE means to buy; it's functionally the wishlist
   ;; TRANSIT means it's in the mail but not here yet
   ;; SELL means you want to get rid of it, put it up on Craigslist
   ;; LOANED means someone currently has it
   ;; OWN is for stuff you actually own (may be overlap for reference and own)
   ;; GIFTED is given to someone as a gift
   ;; SOLD is sold to someone
   ;; DISCARDED is for thrown out
   (sequence "PURCHASE(p)" "TRANSIT(u@/!)" "SELL(k@/!)" "LOANED(n@/!)" "|" "OWN(o@/!)" "GIFTED(g@/!)"  "SOLD(c@/!)" "DISCARDED(q@/!)")

   ;; Sequence for MEDIA
   ;; CONSUME means to read (articles, books, quote, etc.), play (games), listen (music), or watch (a series or movie)
   ;; SUBSCRIBE means to add it to a newsreader or list of some sort
   ;; SHARE means to share on G+, Facebook, reddit, blog about, etc.
   ;; REFERENCE is for stuff you don't own but want to be available later
   (sequence "CONSUME(r)" "SUBSCRIBE(b@/!)" "SHARE(s@/!)" "|" "REFERENCE(f@/!)")

   ;; Sequence for EVENTS
   ;; VISIT means that there is something you would physically like to do, no dates associated
   ;; PLANNED means it's coming up in the future
   ;; MEETING means a real time meeting, i.e. at work, or on the phone for something official
   ;; VISITED means the event took place and is no longer scheduled
   (sequence "VISIT(v)" "PLANNED(l@/!)" "|" "MEETING(m)" "VISITED(y@/!)")
 ))

;; Mark the time a TODO item changes state
(setq org-log-done 'time)

;; Add a note when the TODO item changes state
(setq org-log-done 'note)
