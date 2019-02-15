;; Capture Templates for TODO tasks
(setq org-capture-templates
      '(

   ;; Templates for the TASKS keyword sequence
   ("t" "Tasks")

   ;; TODO     (t) Todo template
   ("tt" "TODO      (t) Todo" entry (file "ref.org")
    "* TODO %?
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"TODO\"       from \"\"           %U
  :END:" :empty-lines 1)

   ;; WAITING  (w) Waiting template
   ("tw" "WAITING   (w) Waiting" entry (file "ref.org")
    "* WAITING %?
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"WAITING\"    from \"\"           %U
  :END:" :empty-lines 1)

   ;; DELEGATED(e) Delegated template
   ("te" "DELEGATED (e) Delegated" entry (file "ref.org")
    "* DELEGATED %?
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"DELEGATED\"  from \"\"           %U
  :END:" :empty-lines 1)

   ;; CANCELLED(x) Cancelled template
   ("tx" "CANCELLED (x) Cancelled" entry (file "ref.org")
    "* CANCELLED %
  CLOSED: %U
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"TODO\"       from \"\"           %U
  :END:" :empty-lines 1)

   ;; DONE     (d) Done template
   ("td" "DONE      (d) Done" entry (file "ref.org")
    "* DONE %?
  CLOSED: %U
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"DONE\"       from \"\"           %U
  :END:" :empty-lines 1)

   ;; Templates for the POSSESSIONS keyword sequence
   ("p" "Possessions")

   ;; PURCHASE (p) Purchase template
   ("pp" "PURCHASE  (p) Purchase" entry (file "ref.org")
    "* PURCHASE %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"PURCHASE\"   from \"\"           %U
  :END:")

   ;; PURCHASED(j) Purchased template
   ("pj" "PURCHASED (j) Purchased" entry (file "ref.org")
    "* PURCHASED %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"PURCHASED\"  from \"\"           %U
  :END:" :empty-lines 1)

   ;; TRANSIT  (u) Transit template
   ("pu" "TRANSIT   (u) Transit" entry (file "ref.org")
    "* TRANSIT %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"TRANSIT\"    from \"\"           %U
  :END:" :empty-lines 1)

   ;; GIFT     (h) Gift    template
   ("ph" "GIFT      (h) Gift" entry (file "ref.org")
    "* GIFT %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"GIFT\"       from \"\"           %U
  :END:" :empty-lines 1)

   ;; SELL     (k) Sell template
   ("pk" "SELL      (k) Sell" entry (file "ref.org")
    "* SELL %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"SELL\"       from \"\"           %U
  :END:" :empty-lines 1)

   ;; LOANED   (n) Loaned template
   ("pj" "LOANED    (n) Loaned" entry (file "ref.org")
    "* LOANED %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"LOANED\"     from \"\"           %U
  :END:" :empty-lines 1)

   ;; UNWANTED (j) Unwanted template
   ("pa" "UNWANTED  (a) Unwanted" entry (file "ref.org")
    "* UNWANTED %?
  CLOSED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
    :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"UNWANTED\"   from \"\"           %U
  :END:" :empty-lines 1)

   ;; OWN      (o) Own template
   ("po" "OWN       (o) Own" entry (file "ref.org")
    "* OWN %?
  CLOSED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"OWN\"        from \"\"           %U
  :END:" :empty-lines 1)

   ;; GIFTED   (g) Gifted template
   ("pg" "GIFTED    (g) Gifted" entry (file "ref.org")
    "* GIFTED %?
  CLOSED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"GIFTED\"     from \"\"           %U
  :END:" :empty-lines 1)

   ;; SOLD     (k) Sold template
   ("pc" "SOLD      (c) Sold" entry (file "ref.org")
    "* SOLD %?
  CLOSED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Merchant:
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"SOLD\"       from \"\"           %U
  :END:" :empty-lines 1)

   ;; DISCARDED(q) Purchased template
   ("pq" "DISCARDED (q) Purchased" entry (file "ref.org")
    "* DISCARDED %?
  CLOSED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Quantity:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"DISCARDED\"  from \"\"           %U
  :END:" :empty-lines 1)

   ;; Templates for the MULTIMEDIA keyword sequence
   ("m" "Multimedia")

   ;; CONSUME  (r) Consume template
   ("mr" "CONSUME   (r) Consume org-protocol" entry (file "ref.org")
    "* CONSUME [[%:link][%:description]]
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"CONSUME\"    from \"\"           %U
  :END:

  %:initial" :empty-lines 1)

   ;; SUBSCRIBE(r) Subscribe template
   ("mb" "SUBSCRIBE (b) Subscribe org-protocol" entry (file "ref.org")
    "* SUBSCRIBE [[%:link][%:description
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"SUBSCRIBE\"  from \"\"           %U
  :END:

  %:initial" :empty-lines 1)

   ;; CONSUMING(l) Consuming template
   ("ml" "CONSUMING (l) Consuming org-protocol" entry (file "ref.org")
    "* CONSUMING [[%:link][%:description]]
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"CONSUMING\"  from \"\"           %U
  :END:

  %:initial" :empty-lines 1)

   ;; SHARE    (s) Share template
   ("ms" "SHARE     (s) Share org-protocol" entry (file "ref.org")
    "* SHARE [[%:link][%:description
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"SHARE\"      from \"\"           %U
  :END:

  %:initial" :empty-lines 1)

   ;; IGNORED  (r) Ignored template
   ("mi" "IGNORED   (i) Ignored org-protocol" entry (file "ref.org")
    "* IGNORED [[%:link][%:description]]
  CLOSED: %U
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"IGNORED\"    from \"\"           %U
  :END:

  %:initial" :empty-lines 1)

   ;; REFERENCE(f) Reference template
   ("mf" "REFERENCE (f) Reference org-protocol" entry (file "ref.org")
    "* REFERENCE [[%:link][%:description]]
  CLOSED: %U
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:description
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"REFERENCE\"  from \"\"           %U
  :END:

  %:initial" :empty-lines 1)

   ;; These templates are used with the EVENTS TODO sequence
   ("e" "Events")

   ;; VISIT    (v) Visit template
   ("ev" "VISIT     (v) Visit" entry (file "ref.org")
    "* VISIT %?
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"VISIT\"      from \"\"           %U
  :END:
  %^t--%^t" :empty-lines 1)

   ;; DIDNOTGO (z) Didnotgo template
   ("ez" "DIDNOTGO  (z) Didnotgo" entry (file "ref.org")
    "* DIDNOTGO %?
  CLOSED: %U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"DIDNOTGO\"   from \"\"           %U
  :END:
  %^t--%^t" :empty-lines 1)

   ;; MEETING  (m) Meeting template
   ("em" "MEETING   (m) Meeting" entry (file "ref.org")
    "* MEETING %?
  CLOSED: %^U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"MEETING\"    from \"\"           %U
  :END:
  %^T--%^T" :empty-lines 1)

   ;; VISITED  (y) Visited template
   ("ey" "VISITED   (y) Visited" entry (file "ref.org")
    "* VISITED %?
  CLOSED: %U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"VISITED\"    from \"\"           %U
  :END:
  %^t--%^t" :empty-lines 1)

   ("n" "Non-TODO States")
   ;;          (a) Account template
  ("na" "          (a) Account" entry (file+headline "org.org" "Accounts")
    "* %?
  :PROPERTIES:
  :Website:
  :Username:
  :Email:
  :Password: %(my/generate-openssl-password)
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:" :empty-lines 1)

   ;;          (b) Business template
   ("nb" "          (b) Business" entry (file+headline "peo.org" "Businesses")
    "* %^{Company} %?
  :PROPERTIES:
  :Company:  %\\1
  :Phone:    %^{Phone}
  :Email:    %^{Email}
  :Website:  %^{Website}
  :Address:  %^{Address}
  :City:     %^{City}
  :State:    %^{State}
  :Zip:      %^{Zip}
  :Map:      [[google-maps:%\\5+%\\6+%\\7+%\\8][Google Maps]]
  :Wifi:
  :Pass:
  :Hours:
  :Yelp:     [[yelp-business:%^{Yelp}][%\\9]]
  :Facebook:
  :G_Plus:
  :Instagram:
  :Linkedin:
  :Twitter:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:" :empty-lines 1)

   ;;          (c) Contact template
   ("nc" "          (c) Contact" entry (file+headline "peo.org" "People")
    "* %^{First} %^{Last}%?
  :PROPERTIES:
  :First:    %\\1
  :Middle:
  :Last:     %\\2
  :Birthday: %^{Birth Date}u
  :Phone:    %^{Phone}
  :Email:    %^{Email}
  :Website:
  :Address:  %^{Address}
  :City:     %^{City}
  :State:    %^{State}
  :Zip:      %^{Zip}
  :Map:      [[google-maps:%\\5+%\\6+%\\7+%\\8][Google Maps]]
  :Company:
  :W-Group:
  :W-Title:
  :W-Phone:
  :W-Email:
  :W-Website:
  :W-Address:
  :W-Office:
  :W-City:
  :W-State:
  :W-Zip:
  :W-Map:
  :Facebook:
  :G:
  :G-Plus:
  :G-Scho:
  :Github:
  :Instagram:
  :Linkedin:
  :OkCupid:
  :Reddit:
  :Twitter:
  :Yelp:
  :YouTube:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:

** Wish List
   :LOGBOOK:
   - State \"TODO\"       from \"\"           %U
   :END:

** Tasks
   :LOGBOOK:
   - State \"TODO\"       from \"\"           %U
   :END:

*** TODO Wish %\\1 %\\2 a Happy Birthday
    DEADLINE: %^{Birthday}t
    :PROPERTIES:
    :Via:
    :Note:
    :END:
    :LOGBOOK:
    - State \"TODO\"       from \"\"           %U
    :END:

**** TODO Buy %\\1 %\\2 a Birthday Gift
     SCHEDULED: %^{Buy Gift By}t DEADLINE: %^{Birthday}t
     :PROPERTIES:
     :Via:
     :Note:
     :END:
     :LOGBOOK:
     - State \"TODO\"       from \"\"           %U
     :END:

*** TODO Buy %\\1 %\\2 a Christmas Gift
    SCHEDULED: <2016-12-01 Tue +1y> DEADLINE: <2016-12-25 Fri +1y>
    :PROPERTIES:
    :Via:
    :Note:
    :END:
    :LOGBOOK:
    - State \"TODO\"       from \"\"           %U
    :END:" :empty-lines 1)

   ;;          (e) Payment template
   ("ne" "          (e) Payment" entry (file "ref.org")
    "* Paid %? :fin:
  :PROPERTIES:
  :Cost:     %^{Cost}
  :Paid:     %^{Paid}
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\3]]
  :Merchant: [[peo:%^{Merchant}][%\\4]]
  :Link:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T" :empty-lines 1)

   ;;          (f) Fuel template
   ("nf" "          (f) Fuel" entry (file+headline "fin.org" "Fuel Up")
    "* Fuel Up at %^{Merchant|Costco Poway|Costco Mission Valley San Diego} :fin:
  :PROPERTIES:
  :Cost:     %^{Cost}
  :Paid:     %\\2
  :Method:   [[fin:%^{Method|Wells Fargo Debit Account|Wells Fargo Credit Account}][%\\3]]
  :Per_Gal:  %^{Per Gallon}
  :Gallons:  %^{Gallons}
  :Beg_Mil:  %?
  :End_Mil:  %^{End Miles}
  :Tot_Mil:
  :MPG:
  :PPM:
  :Merchant: [[peo:%\\1][%\\1]]
  :Link:     [[val:fin/Receipts/%<%Y-%m-%d> %^{Merchant Short Name}.pdf][%<%Y-%m-%d> %\\7.pdf]]
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T" :empty-lines 1)

   ;;          (n) Note template
   ("nn" "          (n) Note" entry (file "ref.org")
    "* %? :note:
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:" :empty-lines 1)

   ;;          (h) Heading template
   ("nh" "          (h) Heading" entry (file "ref.org")
    "* %?
  :PROPERTIES:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:" :empty-lines 1)

   ;;          (j) Journal template
   ("nj" "          (j) Journal" entry (file+datetree "jnl.org")
    "* Journal :org:
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %t\n\n  %?" :empty-lines 1)

   ;;          (p) Paycheck template
   ("np" "          (p) Paycheck" entry (file+headline "fin.org" "Paycheck")
    "* Paycheck :fin:
  :PROPERTIES:
  :Cost:     -%^{Amount}
  :Paid:     -%\\1
  :Method:   [[fin:Wells Fargo Debit Account][Wells Fargo Debit Account]]
  :Merchant: [[peo:General Atomics Aeronautical Systems Inc.][General Atomics Aeronautical Systems Inc.]]
  :Link:     [[val:fin/Banking/Work/General Atomics Aeronautical Systems Inc./Paycheck/%<%Y-%m-%d>.pdf][%<%Y-%m-%d>.pdf]]
  :Note:     %?
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T" :empty-lines 1)

   ;;          (s) Shopping template
   ("ns" "          (s) Shopping" entry (file "ref.org")
    "* %^{Action|Paid|Shopped at|Ate at|Drank at} %^{Merchant|Sprouts Hillcrest San Diego|Trader Joe's Hillcrest San Diego|Trader Joe's Mira Mesa San Diego|Farmer's Market Hillcrest San Diego|Costco Poway|Costco Mission Valley San Diego|Target Mission Valley San Diego|Poncho Villa North Park San Diego|VONS Poway|Ralphs Hillcrest San Diego|Whole Foods Hillcrest San Diego} :fin:
  :PROPERTIES:
  :Cost:     %^{Cost}
  :Paid:     %^{Paid}
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\5]]
  :Merchant: [[peo:%\\2][%\\2]]
  :Link:     %?
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T

  | Item                           | Price ($) | Amount    | Total ($) |
  |                                | <9>       | <9>       | <9>       |
  |--------------------------------+-----------+-----------+-----------|
  |                                |           |           |           |
  |                                |           |           |           |
  |--------------------------------+-----------+-----------+-----------|
  | Tax                            |           | 1         |           |
  | Total                          |           |           |           |
  #+TBLFM: $4=$2*$3;%.2f::@>$4=vsum(@3..@-1);%.2f
  " :empty-lines 1)

   ;;          (t) Transfer template
   ("nt" "          (t) Transfer" entry (file "ref.org")
    "* Transferred %? :fin:
  :PROPERTIES:
  :Cost:     %^{Cost}
  :Paid:     0.00
  :Method:   [[fin:%^{Method|Wells Fargo Debit Account|Wells Fargo Savings Account|Wells Fargo Credit Account|Wells Fargo Checking Account|GE Capital Credit Card}][%\\2]]
  :Merchant: [[fin:%^{Merchant|Wells Fargo Checking Account|Wells Fargo Savings Account|Wells Fargo Credit Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\3]]
  :Link:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %T" :empty-lines 1)

   ("o" "Org-Protocol")
   ;; TODO     (t) Org-protocol todo template
   ;; Alternatively use [[%:link][%:description]] for :Via:
   ("ot" "TODO      (t) Org-Protocol Todo" entry (file "ref.org")
    "* TODO %?
  :PROPERTIES:
  :Via:      %:annotation
  :Note:
  :END:
  :LOGBOOK:
  - State \"TODO\"       from \"\"           %U
  :END:" :empty-lines 1)

   ;; MEETING  (m) Meeting template
   ("om" "MEETING   (m) Org-Protocol Meeting" entry (file "ref.org")
    "* MEETING %:description
  CLOSED: %^U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location: %?
  :Via:      %:annotation
  :Note:
  :END:
  :LOGBOOK:
  - State \"MEETING\"    from \"\"           %U
  :END:
  %^T" :empty-lines 1)

   ;; REFERENCE(f) Reference template
   ("ow" "Web site" entry
    (file "ref.org")
    "* %a :website:\n\n%U %?\n\n%:initial")

   ))

;; Add ID automatically on capture
(add-hook 'org-capture-prepare-finalize-hook 'org-id-store-link)

;; Create separate frame for capture
;; (defun my/make-capture-frame ()
;;   "Create a new frame for org-capture to use."
;;   (select-frame (make-frame '((name . "capture")))))

;; (defadvice org-capture-finalize (after delete-capture-frame activate)
;;   "Advise org-capture-finalize to close the frame if it is the capture frame"
;;   (if (equal "capture" (frame-parameter nil 'name))
;;       (delete-frame)))

;; (defadvice org-capture-destroy (after delete-capture-frame activate)
;;   "Advise org-capture-destroy to close the frame if it is the capture frame"
;;   (if (equal "capture" (frame-parameter nil 'name))
;;       (delete-frame)))

;; (add-hook 'org-capture-mode-hook 'delete-other-windows)
;; (add-hook 'org-capture-mode-hook '(lambda () (setq mode-line-format nil)))

;; (defun my/capture-todo ()
;;   "Capture a TODO item"
;;   (interactive)
;;   (my/make-capture-frame)
;;   (org-capture))

(defun my/org-capture-during-meeting (task)
  "Capture todo task with or without deadline, populate task :Via: field with meeting task,
and then insert a link in line of the new todo task."
  (interactive "sTask: ")
  (call-interactively 'org-store-link)
  (save-excursion
    (org-insert-heading-respect-content)
    (org-return)
    (org-capture 0)
    (org-previous-visible-heading 1)
    (org-cut-subtree)
    (org-do-demote)
    (org-end-of-line)
    (insert task)
    (let ((parent-task
           ;; ;; This implementation prompts due to the use of 'org-insert-last-stored-link.
           ;; (replace-regexp-in-string "\n" ""
           ;;                           (with-temp-buffer
           ;;                             (org-mode)
           ;;                             (org-insert-last-stored-link 1)
           ;;                             (buffer-string)))))
           ;; ;; This implementation requires 'set-window-buffer due to 'execute-kbd-macro.
           ;; ;; To prevent 'y-or-no-p dialog box, set use-dialog-box to nil.
           (with-temp-buffer
             (save-window-excursion
               (set-window-buffer nil (current-buffer))
               (org-mode)
               (execute-kbd-macro [?\C-c ?\C-l return return]))
             (buffer-string))))
      (org-set-property "Via" parent-task))
    (call-interactively 'org-store-link)
    (if (y-or-n-p "Set deadline?")
        (call-interactively 'org-deadline))
    (if (y-or-n-p "Set scheduled?")
        (call-interactively 'org-schedule))
    (org-cycle))
  ;; (org-insert-last-stored-link 1)
  (execute-kbd-macro [?\C-c ?\C-l return return]))
  ;;(org-delete-backward-char 1))

(define-key org-mode-map "\C-cm" 'my/org-capture-during-meeting)

;; Redefine org-cut-special to also exit org-capture
(defun my/org-cut-special-and-exit-org-capture ()
  (interactive)
  (org-cut-special)
  ;; ;; This doesn't work as intended
  ;; (org-capture-kill)
  (kill-buffer)
  (delete-window))

(require 'org-capture)
(define-key org-capture-mode-map (kbd "C-c C-x C-w") 'my/org-cut-special-and-exit-org-capture)

(defun my/generate-openssl-password ()
  "Automatically generate a 15 character password using OpenSSL for the Account capture template."
  (replace-regexp-in-string "\n\\'" ""
                            (shell-command-to-string "openssl rand -base64 15")))

(defun my/insert-openssl-password ()
  "Insert an OpenSSL password from `my/generate-openssl-password' as a string."
  (interactive)
  (insert (my/generate-openssl-password)))
