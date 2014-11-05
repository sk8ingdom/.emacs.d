;; Capture Templates for TODO tasks
(setq org-capture-templates
 '(

   ;; Templates for the TASKS keyword sequence
   ("t" "Tasks")

   ;; TODO     (t) Todo template
   ("tt" "TODO      (t) Todo" entry (file "ref.org")
    "* TODO %?\n  CREATED: %U
  :PROPERTIES:
  :Note:
  :END:")

   ;; Templates for the POSSESSIONS keyword sequence
   ("p" "Possessions")

   ;; PURCHASE (p) Purchase template
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

   ;; Templates for the MULTIMEDIA keyword sequence
   ("m" "Multimedia")

   ;; CONSUME  (r) Consume
   ("mr" "CONSUME   (r) Consume org-protocol" entry (file "ref.org")
    "* CONSUME  [[%:link][%:created]]\n  CREATED: %U
  :PROPERTIES:
  :Creator:  %:creator
  :Created:  %:created
  :Source:   %:source
  :Via:      %:via
  :Link:     %:link
  :Date:     %:date
  :Note:     %:note
  :END:%:quote")

   ;; These templates are used with the EVENTS TODO sequence
   ("e" "Events")

   ;; VISIT    (v) Visit template
   ("ev" "VISIT     (v) Visit" entry (file "ref.org")
    "* VISIT %?\n  CREATED: %U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Note:
  :END:")

   ;; MEETING  (m) Meeting template
   ("em" "MEETING   (m) Meeting" entry (file "ref.org")
    "* MEETING %?\n  %T--%T
  CREATED: %U
  :PROPERTIES:
  :Attend:   [[peo:Dominic Surano][Dominic Surano]]
  :Location:
  :Note:
  :END:

  Notes:
  -

  Tasks:
  -")

   ("n" "Non-to-do States")
   ;;          (a) Account template
  ("na" "          (a) Account" entry (file+headline "org.org" "Accounts")
    "* %?\n  CREATED: %U
  :PROPERTIES:
  :Website:
  :Username:
  :Email:
  :Password:
  :Note:
  :END:")

   ;;          (b) Business template
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

   ;;          (c) Contact template
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

   ;;          (e) Payment template
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

   ;;          (f) Fuel template
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

   ;;          (n) Note template
   ("nn" "          (n) Note" entry (file "ref.org")
    "* %? :note:\n  CREATED: %U
  :PROPERTIES:
  :Note:
  :END:")

   ;;          (h) Paycheck template
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

   ;;          (s) Shopping template
   ("ns" "          (s) Shopping" entry (file "ref.org")
    "* Shop at %? :fin:\n  %t\n  CREATED: %U
  :PROPERTIES:
  :Cost:
  :Paid:
  :Method:
  :Merchant:
  :Note:
  :END:
  | Item                           | Price ($) |    Amount | Total ($) |
  | <30>                           |       <9> |       <9> |       <9> |
  |--------------------------------+-----------+-----------+-----------|
  |                                |           |           |           |
  |                                |           |           |           |
  |--------------------------------+-----------+-----------+-----------|
  | Tax                            |           |           |           |
  | Total                          |           |           |           |
  #+TBLFM: $4=$2*$3::@>$4=vsum(@3..@-1)
  ")

   ("s" "Special Tasks")
   ;; TODO     (g) Project Template
   ("sg" "TODO      (g) Project" entry (file+headline "wrk.org" "Projects")
        "* TODO %?[Platform] [Program] [Analysis Type]\n  CREATED: %U
  :PROPERTIES:
  :For:
  :Other:
  :CN:
  :Note:
  :END:")
 ))
