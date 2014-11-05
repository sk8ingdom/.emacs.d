;;; vvb-mode.el --- a minor mode to display a Visible Vertical Bar(s)

;; Copyright (C) 1999-2002 Jerry G. Chen

;; Author:     Jerry G. Chen <gchen275@yahoo.com>
;; Maintainer: gchen275@yahoo.com
;; Created:    Jan-25-1999
;; Version:    0.40
;; Keywords:   minor-mode, visual highlighting

;; This file may be part of XEmacs.

;; The same license/disclaimer for XEmacs also applies to this package.

;;; Commentary:

;; This package implements a minor mode for XEmacs which, when enabled,
;; will display a transient or permanent visible vertical bar at a user-
;; specified column (ie, `vvb-column'), depending on the value of variable
;; `vvb-permanent-p'.  For a transient one, the bar is visible only when
;; the point is at a column >= `vvb-column'. The bar can also be made
;; "sticky" to the point regardless the value of `vvb-column'. If `vvb-columns'
;; is a list of columns, a set of vertical bars will show (see case #2 below).

;; This minor mode can be used with or without font-lock.

;; To use it, just add this line to your .emacs file:  (require 'vvb-mode)

;; Here are three usage scenarios that I imagine for this minor mode, and
;; the variables to use to satify the requirements:

;; 1. Display a vertical bar always on the column where the point is.
;;    This is useful to see how things are aligned in the file.
;;    -- (setq-default vvb-sticky-p t)

;; 2. Display a vertical bar *permanently* on a particular column.
;;    -- (setq-default vvb-column ?your-preference-here?
;;                     vvb-sticky-p nil
;;                     vvb-permanent-p t)
;;
;;    If you want to display a set of vertical bars across each line, assign
;;    a list of columns to `vvb-columns' instead of `vvb-column'.

;; 3. Display a vertical bar *transiently* on a particular column
;;    only if the point is past over that column.
;;    -- (setq-default vvb-column ?your-preference-here?
;;                     vvb-sticky-p nil
;;                     vvb-permanent-p nil)

;; In cases 2 & 3, you can make the bar extended all the way to the end of lines
;; by (setq-default vvb-right-on-eol-p t)

;; You can also change the appearance for the vertical bar by customizing this
;; face variable, `vvb-face'. eg:
;; (set-face-foreground vvb-face "your-fg-color-name")
;; (set-face-background vvb-face "your-bg-color-name")

;;; Caveats/Bugs:

;; This minor-mode was developed and used in XEmacs v21.0b60. It's also tested to be
;; working for XEmacs 20,4, 19.16 & 19.14. Since XEmacs-specific extents are used, 
;; it will not work out-of-box for any version of FSF GNU Emacs.

;; The vertical bar doesn't show up on lines which are shorter than the target column.
;; And it is not really vertical when it go through the middle of a TAB char, or when
;; variable-width fonts are used.

;;; Change Log:
;; 0.10 01/25/1999
;;      initial release on comp.emacs.xemacs
;; 0.20 01/29/1999
;;      - added `vvb-sticky-p' to allow the bar following the point
;;      - added `vvb-permanent-p' to allow the bar to be transient or permanent
;;      - added `vvb-map' to allow special key bindings for the visible vertical bar
;;      - added context popup menu `vvb-menu' via right-click
;; 0.21 02/01/1999
;;      - fixed use of `get-buffer-window-list' for 20.4, and added this defun
;;        for 19.xx (stolen from 21.0's subr.el)
;; 0.22 02/03/1999
;;      - added defun `vvb-set-column'
;;      - added variable `vvb-columns' for display of multiple vertical bars
;; 0.30 07/18/2000
;;      - added to optionally hilite line on `point' using `vvb-horizontal-p'
;; 0.40 04/26/2002
;;      - added defuns `vvb-toggle-multi-columns' and `vvb-set-multi-columns'
;;      - fixed a bug in `vvb-set-column' thanks to Vilvio Picano <spicano@ptdcs2.intel.com>
;;      - minor enhancements to menubar
;; 0.41 05/09/2002
;;      - minor fix on attaching this mode to minor mode list to avoid duplicates
;;        thanks to Martin K�hmer <martink@demag.rwth-aachen.de>

;;; Code:
(require 'cl)

(defconst vvb-version "v0.40 2002-04-26")

(defvar vvb-column 72
  "*The column at which the vertical bar is visible.")
(make-variable-buffer-local 'vvb-column)

(defvar vvb-columns nil
  "*The columns at which the vertical bars are visible.
It should be a list of numbers, and will be used if and only if
   - it's non-nil, and
   - `vvb-sticky-p' is nil, and
   - `vvb-permanent-p' is t, and
   - `vvb-right-on-eol-p' is nil")
(make-variable-buffer-local 'vvb-columns)

(defvar vvb-right-on-eol-p nil
  "*non-nil will make the right side of the vertical bar 
extended to the end of line.")
(make-variable-buffer-local 'vvb-right-on-eol-p)

(defvar vvb-permanent-p t
  "*non-nil makes the vertical bar permanent instead of transient, meaning that the column
is visible only when `point' is over it.
Only effective when `vvb-sticky-p' is nil.")
(make-variable-buffer-local 'vvb-permanent-p)

(defvar vvb-sticky-p nil
  "*non-nil makes the vertical bar sticky to, ie, following the `point',
in which case `vvb-column' & `vvb-right-on-eol-p' have no effect.")
(make-variable-buffer-local 'vvb-sticky-p)

(defvar vvb-horizontal-p nil
  "*non-nil makes line at `point' visible")
(make-variable-buffer-local 'vvb-horizontal-p)

(defvar vvb-face (make-face 'vvb-face)
  "Face used to hilite the vertical bar.")
(set-face-background vvb-face "pink")

(defvar vvb-mode nil
  "*Non-nil means to display the vertical bar at some column.")
(make-variable-buffer-local 'vvb-mode)

(defvar vvb-map (make-sparse-keymap "vvb-keymap")
  "keymap applied to the visible vertical bar")
(define-key vvb-map 'button3 'vvb-popup-menu)

(defvar vvb-internal-cache nil
  "vvb-mode internal cache of extents used.")
(make-variable-buffer-local 'vvb-internal-cache)


(defvar vvb-menu 
  '("Vertical Bar"
    ["Toggle vvb-mode" vvb-mode :active t :style toggle :selected vvb-mode]

    "--"
    ["Horizontal Line" (setq vvb-horizontal-p (null vvb-horizontal-p)) :active vvb-mode :style toggle :selected vvb-horizontal-p]
    ["Sticky Bar" (setq vvb-sticky-p (null vvb-sticky-p)) :active vvb-mode :style toggle :selected vvb-sticky-p]
    ["Permanent Bar" (setq vvb-permanent-p (null vvb-permanent-p)) :active (and vvb-mode (null vvb-sticky-p)) :style toggle :selected vvb-permanent-p]
    ["Extend to EOL" (setq vvb-right-on-eol-p (null vvb-right-on-eol-p)) :active (and vvb-mode (null vvb-sticky-p)) :style toggle :selected vvb-right-on-eol-p]

    "--"
    ["Toggle Multiple Columns" vvb-toggle-multi-columns :active vvb-mode :style toggle :selected vvb-columns]
    ["Set Columns" vvb-set-multi-columns :active vvb-mode]
    ["Add/Del Current Column" vvb-set-column :active (and vvb-mode vvb-columns)]

    )
  "Popup menu for vvb-mode"
)

(defun vvb-popup-menu (ev)
  "Popup vvb-mode menu `vvb-menu'"
  (interactive "e")
  (popup-menu vvb-menu ev)
)

(defun vvb-mode (&optional arg) 
  "Toggle Visible Vertical Bar mode.
With arg, turn this mode on iff arg is positive.
When this mode is enabled, a transient or permanent vertical bar appears 
at column `vvb-column', depending on `vvb-permanent-p'."
  (interactive "P")
  (setq vvb-mode
	(if (null arg) (not vvb-mode)
	  (> (prefix-numeric-value arg) 0)))

  (or vvb-mode
      (vvb-hide))

  (redraw-modeline)
)


(defun vvb-set-column ()
  "Set the `current-column' to be `vvb-column' if `vvb-columns' is nil.
Otherwise, add to `vvb-columns' unless it's in the list, in which the
column is removed from the list. Display the vertical bar(s)."
  (interactive)
  (let ((col (current-column)))
    (setq vvb-permanent-p t
	  vvb-sticky-p nil)
    (if vvb-columns
	(if (memq col vvb-columns)
	    (setq vvb-columns (delete col vvb-columns))
	  (setq vvb-columns (sort (cons col vvb-columns) '<)))
      (setq vvb-column col))
    (vvb-mode 1)
    )
)

(defun vvb-toggle-multi-columns (&optional cols)
  "Toggle displaying multiple columns"
  (interactive)
  (let ((col (current-column)))

    (if vvb-columns
	(setq vvb-columns nil)
      (vvb-mode 1)
      (setq vvb-columns (or cols (list col))
	    vvb-permanent-p t
	    vvb-sticky-p nil
	    vvb-right-on-eol-p nil
	    ))
    )
)

(defun vvb-set-multi-columns ()
  (interactive)
  (let (s)
    (setq s (split-string (read-string "Enter space-separated columns: " 
				       (mapconcat '(lambda(i) (format "%s" i)) vvb-columns " "))))
    (setq vvb-columns nil)
    (vvb-toggle-multi-columns (mapcar 'string-to-number s))
    )
)

(defun vvb-hide ()
  "Hide the vertical bar if any."
  (let ((all vvb-internal-cache)
	x)
    (while (setq x (pop all))
      (delete-extent x))
    (setq vvb-internal-cache nil)
    )
)

;; compatibility for 19.16, 19.14
(if (string-match "^19" emacs-version)
    (defun get-buffer-window-list (&optional buffer minibuf frame)
      "Return windows currently displaying BUFFER, or nil if none.
BUFFER defaults to the current buffer.
See `walk-windows' for the meaning of MINIBUF and FRAME."
      (cond ((null buffer)
	     (setq buffer (current-buffer)))
	    ((not (bufferp buffer))
	     (setq buffer (get-buffer buffer))))
      (let (windows)
	(walk-windows (lambda (window)
			(if (eq (window-buffer window) buffer)
			    (push window windows)))
		      minibuf frame)
	windows)))


(defun vvb-show ()
  "Display a vertical bar on a column.
if its column equals to `vvb-column'. The bar will be
highlighted using `vvb-face'. If `vvb-right-on-eol-p'
is non-nil, then the right side of the bar will be
extended to the end of line."
  (let ((case-fold-search nil)
	(wlst (get-buffer-window-list (current-buffer) 0 'visible))
	w start end
	cols cols-1 col
	)

    (vvb-hide)

    (if (or vvb-sticky-p
	    (and (null vvb-permanent-p)
		 (null vvb-right-on-eol-p))
	    (null vvb-columns))
	(setq cols-1 (list vvb-column))
      (setq cols-1 vvb-columns))

    (save-excursion

      (if vvb-horizontal-p
	  (let (b e x)
	    (beginning-of-line)
	    (setq b (point))
	    (end-of-line)
	    (setq e (point))
	    (setq x (make-extent b e))
	    (push x vvb-internal-cache)
	    (set-extent-property x 'face vvb-face)
	    (set-extent-property x 'keymap vvb-map)
	    (set-extent-property x 'vvb t)
	    ))

      (while (setq w (pop wlst))
	(setq start (window-start w)
	      end (min (buffer-size) (window-end w)))
	(goto-char start)
	(while (<= (point) end)
	  (setq cols cols-1)
	  (while (setq col (pop cols))
	    (move-to-column col)
	    (if (<= col (current-column))
		(let (b e x)
		  (setq b (point))
		  (if (extent-at b nil 'vvb)
		      nil
		    ;;else
		    (if vvb-right-on-eol-p
			(progn (end-of-line)
			       (setq e (point)))
		      (setq e (1+ b)))

		    (setq x (make-extent b e))
		    (push x vvb-internal-cache)

		    (set-extent-property x 'face vvb-face)
		    (set-extent-property x 'keymap vvb-map)
		    (set-extent-property x 'vvb t)))))
	  
	  (or (search-forward "\n" nil t)
	      (setq end -1))))))
)

(defun vvb-post-command-cb ()
  "Callback attached to `post-command-hook' to make `vvb-mode' working."
  (if vvb-mode
      (condition-case nil
	  (if vvb-sticky-p
	      (let ((vvb-column (current-column))
		    (vvb-right-on-eol-p nil))
		(vvb-show))
	    ;;else
	    (if vvb-permanent-p
		(vvb-show)
	      ;;else - keep vvb transient
	      (if (>= (current-column) vvb-column)
		  (vvb-show)
		(vvb-hide))
	      ))
	(error nil)))
)

;; attach it to the XEmacs system if not already.
(or (assq 'vvb-mode minor-mode-alist)
    (add-to-list 'minor-mode-alist 
		 (list 'vvb-mode (cons modeline-mousable-minor-mode-extent " +"))))

(add-hook 'post-command-hook 'vvb-post-command-cb)

(global-set-key "\C-xc" 'vvb-set-column)
(add-submenu '("Tools") vvb-menu)


(provide 'vvb-mode)

;;; File vvb-mode.el ends here.
