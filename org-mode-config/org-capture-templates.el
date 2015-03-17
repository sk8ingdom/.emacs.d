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
  :END:")

   ;; WAITING  (w) Waiting template
   ("tw" "WAITING   (w) Waiting" entry (file "ref.org")
    "* WAITING %?
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"WAITING\"    from \"\"           %U
  :END:")

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
  :END:")

   ;; DELEGATED(e) Delegated template
   ("te" "DELEGATED (e) Delegated" entry (file "ref.org")
    "* DELEGATED %?
  CLOSED: %U
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"DELEGATED\"  from \"\"           %U
  :END:")

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
  :END:")

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
  :Note:
  :END:
  :LOGBOOK:
  - State \"PURCHASED\"  from \"\"           %U
  :END:")

   ;; TRANSIT  (u) Transit template
   ("pu" "TRANSIT   (u) Transit" entry (file "ref.org")
    "* TRANSIT %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Note:
  :END:
  :LOGBOOK:
  - State \"TRANSIT\"    from \"\"           %U
  :END:")

   ;; SELL     (k) Sell template
   ("pk" "SELL      (k) Sell" entry (file "ref.org")
    "* SELL %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Note:
  :END:
  :LOGBOOK:
  - State \"SELL\"       from \"\"           %U
  :END:")

   ;; LOANED   (n) Loaned template
   ("pj" "LOANED    (n) Loaned" entry (file "ref.org")
    "* LOANED %?
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
  :Merchant: [[peo:%^{Merchant}][%\\2]]
  :Link:
  :Note:
  :END:
  :LOGBOOK:
  - State \"LOANED\"     from \"\"           %U
  :END:")

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
  :Note:
  :END:
  :LOGBOOK:
  - State \"UNWANTED\"   from \"\"           %U
  :END:")

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
  :Note:
  :END:
  :LOGBOOK:
  - State \"OWN\"        from \"\"           %U
  :END:")

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
  :Note:
  :END:
  :LOGBOOK:
  - State \"GIFTED\"     from \"\"           %U
  :END:")

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
  :Note:
  :END:
  :LOGBOOK:
  - State \"SOLD\"       from \"\"           %U
  :END:")

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
  :Note:
  :END:
  :LOGBOOK:
  - State \"DISCARDED\"  from \"\"           %U
  :END:")

   ;; Templates for the MULTIMEDIA keyword sequence
   ("m" "Multimedia")

   ;; CONSUME  (r) Consume template
   ("mr" "CONSUME   (r) Consume org-protocol" entry (file "ref.org")
    "* CONSUME [[%:link][%:created]]
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:created
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"CONSUME\"    from \"\"           %U
  :END:%:quote")

   ;; SUBSCRIBE(r) Subscribe template
   ("mb" "SUBSCRIBE (b) Subscribe org-protocol" entry (file "ref.org")
    "* SUBSCRIBE [[%:link][%:created]]
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:created
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"SUBSCRIBE\"  from \"\"           %U
  :END:%:quote")

   ;; CONSUMING(l) Consuming template
   ("mr" "CONSUMING (l) Consuming org-protocol" entry (file "ref.org")
    "* CONSUMING [[%:link][%:created]]
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:created
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"CONSUMING\"  from \"\"           %U
  :END:%:quote")

   ;; SHARE    (s) Share template
   ("ms" "SHARE     (s) Share org-protocol" entry (file "ref.org")
    "* SHARE [[%:link][%:created]]
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:created
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"SHARE\"      from \"\"           %U
  :END:%:quote")

   ;; IGNORED  (r) Ignored template
   ("mi" "IGNORED   (i) Ignored org-protocol" entry (file "ref.org")
    "* IGNORED [[%:link][%:created]]
  CLOSED: %U
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:created
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"IGNORED\"    from \"\"           %U
  :END:%:quote")

   ;; REFERENCE(f) Reference template
   ("mf" "REFERENCE (b) Reference org-protocol" entry (file "ref.org")
    "* REFERENCE [[%:link][%:created]]
  CLOSED: %U
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:created
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:
  :LOGBOOK:
  - State \"REFERENCE\"  from \"\"           %U
  :END:%:quote")

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
  %^T--%^T")

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
  %^T--%^T")

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
  %^T--%^T

  Notes:
  -

  Tasks:
  -")

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
  %^T--%^T")

   ("n" "Non-to-do States")
   ;;          (a) Account template
  ("na" "          (a) Account" entry (file+headline "org.org" "Accounts")
    "* %?
  :PROPERTIES:
  :Website:
  :Username:
  :Email:
  :Password:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:")

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
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:")

   ;;          (c) Contact template
   ("nc" "          (c) Contact" entry (file+headline "peo.org" "People")
    "* %^{First} %^{Last}%? %^g
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
     DEADLINE: %^{Birthday}t
     :PROPERTIES:
     :Via:
     :Note:
     :END:
     :LOGBOOK:
     - State \"TODO\"       from \"\"           %U
     :END:

*** TODO Buy %\\1 %\\2 a Christmas Gift
    DEADLINE: <2015-12-25 Fri +1y>
    :PROPERTIES:
    :Via:
    :Note:
    :END:
    :LOGBOOK:
    - State \"TODO\"       from \"\"           %U
    :END:")

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
  %t")

   ;;          (f) Fuel template
   ("nf" "          (f) Fuel" entry (file+headline "fin.org" "Fuel Up")
    "* Fuel Up at %^{Merchant|Costco Poway|Costco Mission Valley San Diego} %^{YYYY-MM-DD} :fin:
  :PROPERTIES:
  :Cost:     %^{Cost}
  :Paid:     %\\3
  :Method:   [[fin:%^{Method|Wells Fargo Debit Account|Wells Fargo Credit Account}][%\\4]]
  :Per_Gal:  %^{Per Gallon}
  :Gallons:  %^{Gallons}
  :Beg_Mil:  %?
  :End_Mil:  %^{End Miles}
  :Tot_Mil:
  :MPG:
  :PPM:
  :Merchant: [[peo:%\\1][%\\1]]
  :Link:     [[val:fin/Receipts/%\\2 %^{Merchant Short Name}.pdf][%\\2 %\\8.pdf]]
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %t")

   ;;          (n) Note template
   ("nn" "          (n) Note" entry (file "ref.org")
    "* %? :note:
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:")

   ;;          (h) Heading template
   ("nh" "          (h) Heading" entry (file "ref.org")
    "* %?
  :PROPERTIES:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:")

   ;;          (j) Journal template
   ("nj" "          (j) Journal" entry (file+headline "org.org" "Write Daily Journal")
    "* Journal %^{YYYY-MM-DD} :org:
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:\n\n   %?")

   ;;          (p) Paycheck template
   ("np" "          (p) Paycheck" entry (file+headline "fin.org" "Paycheck")
    "* Paycheck %^{YYYY-MM-DD} :fin:
  :PROPERTIES:
  :Cost:     -%^{Amount}
  :Paid:     -%\\2
  :Method:   [[fin:Wells Fargo Debit Account][Wells Fargo Debit Account]]
  :Merchant: [[peo:General Atomics Aeronautical Systems Inc.][General Atomics Aeronautical Systems Inc.]]
  :Link:     [[val:fin/Banking/Work/General Atomics Aeronautical Systems Inc./Paycheck/%\\1.pdf][%\\1.pdf]]
  :Note:     %?
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %t")

   ;;          (s) Shopping template
   ("ns" "          (s) Shopping" entry (file "ref.org")
    "* Shop at %^{Merchant|Sprouts Hillcrest San Diego|Trader Joe's Hillcrest San Diego|Trader Joe's Mira Mesa San Diego|Farmer's Market Hillcrest San Diego|Costco Poway|Costco Mission Valley San Diego|Target Mission Valley San Diego|Poncho Villa North Park San Diego|VONS Poway|Ralphs Hillcrest San Diego|Whole Foods Hillcrest San Diego} %^{YYYY-MM-DD} :fin:
  :PROPERTIES:
  :Cost:     %^{Cost}
  :Paid:     %\\3
  :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\4]]
  :Merchant: [[peo:%\\1][%\\1]]
  :Note:     %?
  :Link:     [[val:fin/Receipts/%\\2 %^{Merchant Short Name}.pdf][%\\2 %\\5.pdf]]
  :END:
  :LOGBOOK:
  - State \"\"           from \"\"           %U
  :END:
  %t

  | Item                           | Price ($) |    Amount | Total ($) |
  | <30>                           |       <9> |       <9> |       <9> |
  |--------------------------------+-----------+-----------+-----------|
  |                                |           |           |           |
  |                                |           |           |           |
  |--------------------------------+-----------+-----------+-----------|
  | Tax                            |           |         1 |           |
  | Total                          |           |           |           |
  #+TBLFM: $4=$2*$3::@>$4=vsum(@3..@-1)
  ")))

;; Add ID automatically on capture
(add-hook 'org-capture-prepare-finalize-hook 'org-id-store-link)
