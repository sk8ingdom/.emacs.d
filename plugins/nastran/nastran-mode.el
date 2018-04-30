;; (require 'vvb-mode)
;; (load "~/.emacs.d/plugins/vvb-mode")

;; (setq-default vvb-columns '(9 17 25 33 41 49 57 65 73 81)
;;               vvb-sticky-p nil
;;               vvb-permanent-p t)

;; (add-to-list 'auto-mode-alist '("\\.bdf\\'" . nastran-mode))
;; (add-to-list 'auto-mode-alist '("\\.dat\\'" . nastran-mode))
;; (add-to-list 'auto-mode-alist '("\\.bas\\'" . nastran-mode))

;; End of File
(defun nastran-fill-last-column ()
  (load "~/.emacs.d/plugins/fill-column-indicator.el")
  ;; (setq fci-rule-column 80)
  (setq fill-column 80))

;; Tabs
(setq indent-tabs-mode nil)
(setq tab-stop-list '(0 8 16 24 32 40 48 56 64 72 80))

(defvar nastran-map
  (let ((map (make-keymap)))
    (define-key map [tab] 'move-to-tab-stop)
    (define-key map [backtab] 'nastran-backward-move-to-tab-stop)
    map)
  "Keymap for Nastran major mode")

;; https://groups.google.com/forum/#!topic/gnu.emacs.help/AQTq4AwCjOo
(defun nastran-backward-move-to-tab-stop ()
  "Move point to previous (greatest less than point) tab-stop.  The
variable `tab-stop-list' is a list of columns at which there are tab
stops. Use \\[edit-tab-stops] to edit tab stops interactively.  This
is a move-backward version of \\[move-to-tab-stop]."
  (interactive)
  ;; loop to find greatest tab stop less than point
  (let ((tabs (reverse tab-stop-list)))
    (while (and tabs (<= (current-column) (car tabs)))
      (setq tabs (cdr tabs)))
    ;; if tabs not nil, car tabs is that column
    ;; Otherwise, column should be 0.
    ;; So go there.

    (cond (tabs (move-to-column (car tabs) t))
          (t  (move-to-column 0 t)))))

;; Columns
(defvar nastran-columns '(8 16 24 32 40 48 56 64 72))

(defvar nastran-column-width 8)

(defun nastran-columns ()
  (interactive)
  (load "~/.emacs.d/plugins/column-marker")
  ;; (while nastran-columns
  ;;   (column-marker-create (intern (concat "column-marker-" (number-to-string (car nastran-columns)))))
  ;;   ((intern (concat "column-marker-" (number-to-string (car nastran-columns)))) (car nastran-columns))
  ;;   (setq nastran-columns (cdr nastran-columns))))
  ;;
  (column-marker-create column-marker-8)
  (column-marker-8 8)
  (column-marker-create column-marker-9)
  (column-marker-9 9)
  (column-marker-create column-marker-10)
  (column-marker-10 10)
  (column-marker-create column-marker-11)
  (column-marker-11 11)
  (column-marker-create column-marker-12)
  (column-marker-12 12)
  (column-marker-create column-marker-13)
  (column-marker-13 13)
  (column-marker-create column-marker-14)
  (column-marker-14 14)
  (column-marker-create column-marker-15)
  (column-marker-15 15)
  ;;
  (column-marker-create column-marker-24)
  (column-marker-24 24)
  (column-marker-create column-marker-25)
  (column-marker-25 25)
  (column-marker-create column-marker-26)
  (column-marker-26 26)
  (column-marker-create column-marker-27)
  (column-marker-27 27)
  (column-marker-create column-marker-28)
  (column-marker-28 28)
  (column-marker-create column-marker-29)
  (column-marker-29 29)
  (column-marker-create column-marker-30)
  (column-marker-30 30)
  (column-marker-create column-marker-31)
  (column-marker-31 31)
  ;;
  (column-marker-create column-marker-40)
  (column-marker-40 40)
  (column-marker-create column-marker-41)
  (column-marker-41 41)
  (column-marker-create column-marker-42)
  (column-marker-42 42)
  (column-marker-create column-marker-43)
  (column-marker-43 43)
  (column-marker-create column-marker-44)
  (column-marker-44 44)
  (column-marker-create column-marker-45)
  (column-marker-45 45)
  (column-marker-create column-marker-46)
  (column-marker-46 46)
  (column-marker-create column-marker-47)
  (column-marker-47 47)
  ;;
  (column-marker-create column-marker-56)
  (column-marker-56 56)
  (column-marker-create column-marker-57)
  (column-marker-57 57)
  (column-marker-create column-marker-58)
  (column-marker-58 58)
  (column-marker-create column-marker-59)
  (column-marker-59 59)
  (column-marker-create column-marker-60)
  (column-marker-60 60)
  (column-marker-create column-marker-61)
  (column-marker-61 61)
  (column-marker-create column-marker-62)
  (column-marker-62 62)
  (column-marker-create column-marker-63)
  (column-marker-63 63)
  ;;
  (column-marker-create column-marker-72)
  (column-marker-72 72)
  (column-marker-create column-marker-73)
  (column-marker-73 73)
  (column-marker-create column-marker-74)
  (column-marker-74 74)
  (column-marker-create column-marker-75)
  (column-marker-75 75)
  (column-marker-create column-marker-76)
  (column-marker-76 76)
  (column-marker-create column-marker-77)
  (column-marker-77 77)
  (column-marker-create column-marker-78)
  (column-marker-78 78)
  (column-marker-create column-marker-79)
  (column-marker-79 79))

(defun nastran-link-includes ()
  (interactive)
  (load "~/.emacs.d/plugins/button-lock.el")
  (global-button-lock-mode t)
  (button-lock-set-button "'.*'"
                          'find-file-at-point
                          :face 'link :face-policy 'prepend)
  )

;; Syntax highlighting
(setq nastran-keyword-elements '("CGAP" "CHEXA" "CMASS2" "CONM" "CONM2" "CONROD" "CPENTA" "CQUAD4" "CQUAD8" "CROD" "CTETRA" "CTRIA3" "CTRIA6" "CWELD" "GENEL" "PLOTEL" "RBAR" "RBE2" "RBE3" "CELAS2" "CELAS1" "CBAR" "CBEAM" "CBUSH"))
(setq nastran-keyword-proprietes '("CORD1C" "CORD1R" "CORD1S" "CORD2C" "CORD2R" "CORD2S" "MAT1" "MAT2" "MAT4" "MAT8" "MATS1" "PBAR" "PBARL" "PBEAM" "PBUSH" "PCOMP" "PELAS" "PGAP" "PSHELL" "PSOLID" "PWELD"))
(setq nastran-keyword-spc '("AUTOSPC" "BILIN" "GRDPNT" "K" "POST" "PRTMAXIM" "UD" "VONMISES" "Z" "ALIGN" "ALL" "AUTO" "BCONP" "BFRIC" "BLSEG" "BOUTPUT" "BWIDTH" "BY" "CORNER" "ELEMID" "ENDT" "FORM" "G" "GRID" "K6ROT" "LGDISP" "LOG" "LOGICAL" "MAXRATIO" "NEWSEQ" "NO" "NOCOMPS" "NONE" "OFF" "OGEOM" "ON" "OUTPUT2" "OUTPUT4" "BUFFSIZE" "PATVER" "PHASE" "PLASTIC" "PLOT" "PRINT" "PROJECT" "PUNCH" "Q" "QQ" "QT" "REAL" "SNORM" "SORT1" "SORT2" "SPOT" "SYSTEM" "T" "THRU" "TQ" "TT" "UNIT" "XYPLOT" "YES" "SYM" "STATUS" "OPTEXIT" "CRIT" "THRESH" "SPARSEDR" "DFREQ"))
(setq nastran-keyword-load '("ANALYSIS" "DESSUB" "ACCEL" "ACCELERATION" "ASET1" "DAREA" "DLOAD" "FREQ" "FREQ1" "FREQ5" "RANDOM" "RLOAD2" "TABLED" "DISP" "DISPLACEMENT" "ECHO" "EIGR" "EIGRL" "ELFORCE" "FORCE" "GRAV" "LABEL" "LOAD" "MAXLINES" "METHOD" "MOMENT" "MPC" "MPCADD" "MPCFORCES" "OLOAD" "OUTPUT" "PLOAD4" "RANDPS" "REPCASE" "SET" "SPC" "SPC" "SPC1" "SPCADD" "SPCD" "SPCFORCE" "SPCFORCES" "STRESS" "SUBCASE" "SUBTITLE" "SUPORT" "TABLED1" "TABRND1" "TEMP" "TEMPD" "TITLE" "XYPRINT" "GROUNDCHECK" "SDAMPING" "RESVEC"))
(setq nastran-keyword-param  '("ALTER" "ASSIGN" "COMPILE" "COPY" "INPUTT4" "SEALL" "SUPER" "TYPE" "ADD" "BEGIN" "BULK" "CEND" "DIAG" "DMIIN" "ENDDATA" "EQUIV" "ID" "INCLUDE" "INIT" "MATGEN" "MERGE" "NASTRAN" "NLPARM" "PARAM" "PARAML" "PROJ" "PURGE" "RESTART" "SETVAL" "SOL" "TIME"))
(setq nastran-keyword-noeud '("GRID" "SPOINT"))
(setq nastran-keyword-optimisation '("DESVAR" "DVPREL1" "DVPREL2" "DVMREL1" "DVMREL2" "DRESP1" "DCONSTR"))

(setq nastran-keyword-elements-regexp (regexp-opt nastran-keyword-elements 'words))
(setq nastran-keyword-proprietes-regexp (regexp-opt nastran-keyword-proprietes 'words))
(setq nastran-keyword-spc-regexp (regexp-opt nastran-keyword-spc 'words))
(setq nastran-keyword-load-regexp (regexp-opt nastran-keyword-load 'words))
(setq nastran-keyword-param-regexp (regexp-opt nastran-keyword-param 'words))
(setq nastran-keyword-noeud-regexp (regexp-opt nastran-keyword-noeud 'words))
(setq nastran-keyword-optimisation-regexp (regexp-opt nastran-keyword-optimisation 'words))

(setq nastran-keyword-elements nil)
(setq nastran-keyword-proprietes nil)
(setq nastran-keyword-spc nil)
(setq nastran-keyword-load nil)
(setq nastran-keyword-param nil)
(setq nastran-keyword-noeud nil)
(setq nastran-keyword-optimisation nil)

(setq nastran-font-lock-keywords
      `(
        (,nastran-keyword-elements-regexp . font-lock-type-face)
        (,nastran-keyword-proprietes-regexp . font-lock-constant-face)
        (,nastran-keyword-spc-regexp . font-lock-builtin-face)
        (,nastran-keyword-load-regexp . font-lock-doc-face)
        (,nastran-keyword-param-regexp . font-lock-function-name-face)
        (,nastran-keyword-noeud-regexp . font-lock-negation-char-face)
        (,nastran-keyword-optimisation-regexp . font-lock-reference-face)))

(setq nastran-syntax-table
      (let ((table (make-syntax-table)))
        ;; $ is a comment delimiter
        (modify-syntax-entry ?$ "< b" table)
        (modify-syntax-entry ?\n "> b" table)
        table))

;; (defun nastran-mode ()
;;   (interactive)
;;   (define-derived-mode nastran-mode prog-mode "Simple Nastran Mode"
;;     :syntax-table nastran-syntax-table)
;;   (setq mode-name "nastran")
;;   (setq major-mode
;;         'nastran-mode
;;         mode-name "Nastran"
;;         font-lock-defaults '(nastran-font-lock-keywords)
;;         require-fine-newline t)
;;   (make-local-variable 'font-lock-defaults)
;;   (setq font-lock-defaults '(nastran-font-lock-keywords))
;;   (use-local-map nastran-mode-map)
;;   (run-hooks 'nastran-mode-hook)
;;   (nastran-fill-last-column)
;;   (nastran-columns)
;;   (fci-mode)
;;   (nastran-link-includes))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.blk\\'" . nastran-mode))
(define-derived-mode nastran-mode prog-mode "Simple Nastran Mode"
  :syntax-table nastran-syntax-table
  (setq font-lock-defaults '((nastran-font-lock-keywords)))
  (font-lock-fontify-buffer)
  (use-local-map nastran-map)
  (run-hooks 'nastran-hook)
  (nastran-fill-last-column)
  ;; (nastran-columns)
  (fci-mode)
  (nastran-link-includes))

(provide 'nastran-mode)
;; (run-hooks 'nastran-mode-hook)
