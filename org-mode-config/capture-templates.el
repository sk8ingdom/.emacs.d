;; Capture Templates for TODO tasks
;; Only have templates for org-todo-keywords that don't log time / note on change state and non-keywords
(setq org-capture-templates
 '(

   ;; Templates for the TASKS keyword sequence
   ("t" "Tasks")

   ;; TODO     (t) Todo template     {{{
   ("tt" "TODO      (t) Todo" entry (file "ref.org")
    "* TODO %?\n  CREATED: %U
  :PROPERTIES:
  :Note:
  :END:")
   ;; }}}

   ;; WAITING  (w) Waiting template  {{{
   ("tw" "WAITING   (w) Waiting" entry (file "ref.org")
    "* WAITING %?\n  CREATED: %U
  :PROPERTIES:
  :Note:
  :END:")
   ;; }}}

   ;; CANCELED (x) Cancelled template{{{
   ("tx" "CANCELLED (x) Cancelled" entry (file "ref.org")
    "* CANCELLED %?\n  CREATED: %U
  :PROPERTIES:
  :Note:
  :END:")
   ;; }}}

   ;; DELEGATED(e) Delegated template{{{
   ("te" "DELEGATED (e) Delegated" entry (file "ref.org")
    "* DELEGATED %?\n  CREATED: %U
  :PROPERTIES:
  :Note:
  :END:")
   ;; }}}

   ;; DONE     (d) Done template     {{{
   ("td" "DONE      (d) Done" entry (file "ref.org")
    "* DONE %?\n  CREATED: %U
  :PROPERTIES:
  :Note:
  :END:")
   ;; }}}

   ;; Templates for the POSSESSIONS keyword sequence
   ("p" "Possessions")

   ;; PURCHASE (p) Purchase template {{{
   ("pp" "PURCHASE  (p) Purchase" entry (file "ref.org")
    "* PURCHASE %?\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Link:
  :Note:
  :END:")
   ;; }}}

   ;; TRANSIT  (u) Purchase template {{{
   ("pu" "TRANSIT   (u) Transit" entry (file "ref.org")
    "* TRANSIT %?\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Link:
  :Note:
  :END:")
   ;; }}}

   ;; SELL     (k) Sell template     {{{
   ("pk" "SELL      (k) Sell" entry (file "ref.org")
    "* SELL %?\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Link:
  :Note:
  :END:")
   ;; }}}

   ;; LOANED   (n) Loaned template   {{{
   ("pn" "LOANED    (n) Loaned" entry (file "ref.org")
    "* LOANED %?\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Link:
  :Note:
  :END:")
   ;; }}}

   ;; UNWANTED (a) Unwanted template {{{
   ("pa" "UNWANTED  (a) Unwanted" entry (file "ref.org")
    "* UNWANTED %?\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Link:
  :Note:
  :END:")
   ;; }}}

   ;; OWN      (o) Own template      {{{
   ("po" "OWN       (o) Own" entry (file "ref.org")
    "* OWN %?\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Link:
  :Note:
  :END:")
   ;; }}}

   ;; GIFTED   (g) Gifted template   {{{
   ("pg" "GIFTED    (g) Gifted" entry (file "ref.org")
    "* GIFTED %?\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Link:
  :Note:
  :END:")
   ;; }}}

   ;; SOLD     (c) Sold template     {{{
   ("pc" "SOLD      (c) Sold" entry (file "ref.org")
    "* SOLD %?\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Link:
  :Note:
  :END:")
   ;; }}}

   ;; DISCARDED(q) Discarded template{{{
   ("pq" "DISCARDED (q) Discarded" entry (file "ref.org")
    "* DISCARDED %?\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Link:
  :Note:
  :END:")
   ;; }}}

   ;; Templates for the MULTIMEDIA keyword sequence
   ("m" "Multimedia")

   ;; CONSUME  (r) Consume org-prot  {{{
   ("mr" "CONSUME   (r) Consume org-protocol" entry (file "ref.org")
    "* CONSUME [[%:link][%:description]]\n  CREATED: %U
  :PROPERTIES:
  :Creator:  %?
  :Created:  %:description
  :Source:
  :Link:     %:link
  :Date:     %u
  :Note:
  :END:
  %:initial")
   ;; }}}

   ;; SUBSCRIBE(b) Subscribe org-prot{{{
   ("mb" "SUBSCRIBE (b) Subscribe org-protocol" entry (file "ref.org")
    "* SUBSCRIBE [[%:link][%:description]]\n  CREATED: %U
  :PROPERTIES:
  :Creator:  %?
  :Created:  %:description
  :Source:
  :Link:     %:link
  :Date:     %u
  :Note:
  :END:
  %:initial")
   ;; }}}

   ;; SHARE    (s) Share org-prot    {{{
   ("ms" "SHARE     (s) Share org-protocol" entry (file "ref.org")
    "* SHARE [[%:link][%:description]]\n  CREATED: %U
  :PROPERTIES:
  :Creator:  %?
  :Created:  %:description
  :Source:
  :Link:     %:link
  :Date:     %u
  :Note:
  :END:
  %:initial")
   ;; }}}

   ;; IGNORED  (i) Ignored org-prot  {{{
   ("mi" "IGNORED   (i) Ignored org-protocol" entry (file "ref.org")
    "* IGNORED [[%:link][%:description]]\n  CREATED: %U
  :PROPERTIES:
  :Creator:  %?
  :Created:  %:description
  :Source:
  :Link:     %:link
  :Date:     %u
  :Note:
  :END:
  %:initial")
   ;; }}}

   ;; REFERENCE(f) Reference org-prot{{{
   ("mf" "REFERENCE (f) Reference org-protocol" entry (file "ref.org")
    "* REFERENCE [[%:link][%:description]]\n  CREATED: %U
  :PROPERTIES:
  :Creator:  %?
  :Created:  %:description
  :Source:
  :Link:     %:link
  :Date:     %u
  :Note:
  :END:
  %:initial")
   ;; }}}


   ;; These templates are used with the EVENTS TODO sequence
   ("e" "Events")

   ;; VISIT    (v) Visit template    {{{
   ("ev" "VISIT     (v) Visit" entry (file "ref.org")
    "* VISIT %?\n  CREATED: %U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Note:
  :END:")
   ;; }}}

   ;; PLANNED  (l) Planned template  {{{
   ("el" "PLANNED   (l) Planned" entry (file "ref.org")
    "* PLANNED %?\n  CREATED: %U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Note:
  :END:")
   ;; }}}

   ;; DIDNOTGO (z) Didnotgo template {{{
   ("ez" "DIDNOTGO  (z) DIDNOTGO" entry (file "ref.org")
    "* PLANNED %?\n  CREATED: %U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Note:
  :END:")
   ;; }}}

   ;; MEETING  (m) Meeting template  {{{
   ("em" "MEETING   (m) Meeting" entry (file "ref.org")
    "* MEETING %?\n  %T--%T
  CREATED: %U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Note:
  :END:

  Tasks from meeting:
  -")
   ;; }}}

   ;; VISITED  (y) Visited template  {{{
   ("ey" "VISITED   (v) Visited" entry (file "ref.org")
    "* VISITED %?\n  CREATED: %U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Note:
  :END:")
   ;; }}}

   ("n" "Non-to-do States")
   ;;          (a) Account template  {{{
  ("na" "          (a) Account" entry (file+headline "org.org" "Accounts")
    "* %?\n  CREATED: %U
  :PROPERTIES:
  :Website:
  :Username:
  :Email:
  :Password:
  :Note:
  :END:")
   ;; }}}

   ;;          (b) Business template {{{
   ("nb" "          (b) Business" entry (file+headline "peo.org" "Businesses")
    "* %?\n  CREATED: %U
  :PROPERTIES:
  :Company:
  :W_Phone:
  :W_Email:
  :W_Web:
  :W_Street:
  :W_City:
  :W_State:
  :W_Zip:
  :W_Map:
  :W_Wifi:
  :W_Pass:
  :Hours:
  :Yelp:
  :Facebook:
  :G_Plus:
  :Instagram:
  :Linkedin:
  :Twitter:
  :Note:
  :END:")
   ;; }}}

   ;;          (c) Contact template  {{{
   ("nc" "          (c) Contact" entry (file+headline "peo.org" "People")
    "* %?\n  CREATED: %U
  :PROPERTIES:
  :First:
  :Middle:
  :Last:
  :Birthday:
  :P_Phone:
  :P_Email:
  :P_Web:
  :P_Street:
  :P_City:
  :P_State:
  :P_Zip:
  :P_Map:
  :Company:
  :W_Group:
  :W_Title:
  :W_Phone:
  :W_Email:
  :W_Web:
  :W_Office:
  :W_Street:
  :W_City:
  :W_State:
  :W_Zip:
  :W_Map:
  :Facebook:
  :G:
  :G_Plus:
  :G_Scho:
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
** [First] [Last] Wish List
** TODO Wish [First] [Last] a Happy Birthday
   DEADLINE: +1y
*** TODO Buy [First] [Last] a Birthday Gift
    DEADLINE: +1y
** TODO Buy [First] [Last] a Christmas Gift
   DEADLINE: +1y")
   ;; }}}

   ;;          (e) Payment template  {{{
   ("ne" "          (e) Payment" entry (file "ref.org")
    "* Paid %? :fin:\n  %t\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Link:
  :Note:
  :END:")
   ;; }}}

   ;;          (f) Fuel template     {{{
   ("nf" "          (f) Fuel" entry (file+headline "fin.org" "Fuel Up")
    "* Fuel Up at %? :fin:\n  %t\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Per_Gal:
  :Gallons:
  :Beg_Mil:
  :End_Mil:
  :Tot_Mil:
  :MPG:
  :PPM:
  :Merchant:
  :END:")
   ;; }}}

   ;;          (n) Note template     {{{
   ("nn" "          (n) Note" entry (file "ref.org")
    "* %? :note:\n  CREATED: %U
  :PROPERTIES:
  :Note:
  :END:")
   ;; }}}
   ;;          (h) Paycheck template {{{
   ("nh" "          (h) Paycheck" entry (file+headline "fin.org" "Paycheck")
    "* Paycheck %? :fin:\n  %t\n  CREATED: %U
  :PROPERTIES:
  :Cost:     -
  :Paid:     -
  :Method:   [[fin:Wells%20Fargo%20Debit%20Account][Wells Fargo Debit Account]]
  :Merchant: [[peo:General%20Atomics%20Aeronautical%20Systems%20Inc.][General Atomics Aeronautical Systems Inc.]]
  :Link:     [[val:fin/Banking/Work/General%20Atomics%20Aeronautical%20Systems%20Inc./Paycheck/YYYY-MM-DD.pdf][YYYY-MM-DD.pdf]]
  :Note:
  :END:")
   ;; }}}

   ;;          (s) Shopping template {{{
   ("ns" "          (s) Shopping" entry (file+headline "ref.org" "Shopping")
    "* Shop at %? :fin:\n  %t\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Note:
  :END:
  | Item                           | Price ($) | Amount | Total ($) |
  |--------------------------------+-----------+--------+-----------|
  |                                |           |        |           |
  |                                |           |        |           |
  |--------------------------------+-----------+--------+-----------|
  | Total                          |           |        |           |
  | Total + Tax (based on receipt) |           |        |           |
  #+TBLFM: $4=$2*$3
  ")
   ;; }}}

   ("s" "Special Tasks")
   ;; TODO     (g) Project Template  {{{
   ("sg" "TODO      (g) Project" entry (file+headline "wrk.org" "Projects")
        "* TODO %?[Platform] [Program] [Analysis Type]\n  CREATED: %U
  :PROPERTIES:
  :Subject: [Platform] | [Program] | [Analysis Type]
  :Start_Date:
  :Requestor:
  :Other Contacts:
  :Due_Date:
  :Distribute_To:
  :Date_Complete:
  :Status:
  :Priority:
  :Complete:
  :SAG_Originator:
  :Charge_Number: [XXXX-XXX-XXX]
  :Estimate_Hours:
  :Hours_Charged:
  :Assigned_Eng:
  :Deliverable:
  :Platform:
  :Planning_Sheet:
  :WD_Analysis:
  :WD_Flutter:
  :WD_Testing:
  :Task_Description: Perform flutter analysis on [Platform] with [Payload]
  :Data_Info:
  :Data_Office:
  :Spec_Req: No flutter within the 115% expanded flight envelope
  :Assy_Pod Data Sheets:
  :Assy_Mass Property Sheets:
  :Assy_Dynamic Deck Parts:
  :Assy_ZAERO Parts:
  :FEM_Final: [Config] [Fuel] [MIM] Dynamic FEM
  :Input_Final: [Config] [Fuel] [MIM] ZAERO deck
  :Report_Location:
  :Complete_Result_Summary:
  :Emails_Attachments:
  :Note:
  :END:")
   ;; }}}
 ))
