;;; outlookedit.el
;; use a couple of Tcl scripts to invoke my Tcl scripts to do COM
;; related actions on Outlook to get and replace text in the
;; reply/compose boxes allowing it to be edited in Emacs
;;
;(defvar mno-get-outlook-body
;  "tclsh C:\\Userdata\\Tcl\\grabOutlookMessage.tcl")
;(defvar mno-put-outlook-body
;  "tclsh C:\\Userdata\\Tcl\\putMessageInOutlook.tcl")
(defvar mno-get-outlook-body
  "cscript //B //U //Job:getMessage ~/bin/outlook_emacs.wsf")
(defvar mno-put-outlook-body
  "cscript //B //U //Job:putMessage ~/bin/outlook_emacs.wsf")
(defvar mno-outlook-default-justification 'full)

(global-set-key "\C-coe" 'mno-edit-outlook-message)
(global-set-key "\C-cos" 'mno-save-outlook-message)

(defun mno-edit-outlook-message ()
  "* Slurp an outlook message into a new buffer ready for editing
The message must be in the active Outlook window.  Typically the
user would press the Reply or Reply-all button in Outlook then
switch to Emacs and invoke \\[mno-edit-outlook-message]
Once all edits are done, the function mno-save-outlook-message
(invoked via \\[mno-save-outlook-message]) can be used to send the
newly edited text back into the Outlook window.  It is important that
the same Outlook window is current in outlook as was current when the
edit was started when this command is invoked."
  (interactive)
  (save-excursion
    (let ((buf (get-buffer-create "*Outlook Message*"))
         (body (shell-command-to-string mno-get-outlook-body)))
      (switch-to-buffer buf)
      (message-mode)   ; enables automagic reflowing of text in quoted
                                        ; sections
      (setq default-justification mno-outlook-default-justification)
      (setq body (replace-regexp-in-string "\r" "" body))
      (delete-region (point-min) (point-max))
      (insert body)
      (goto-char (point-min)))))

(defun mno-save-outlook-message ()
  "* Send the outlook message buffer contents back to Outlook current window
Unfortunately, Outlook 2000 then renders this text as Rich Text format
rather than plain text, overriding any user preference for plain text.
The user then needs to select Format->Plain text in the outlook
compose window to reverse this.
Outlook 2002 apparently has a BodyFormat parameter to control this."
  (interactive)
  (save-excursion
    (let ((buf (get-buffer "*Outlook Message*")))
      (set-buffer buf)
      (shell-command-on-region (point-min) (point-max) mno-put-outlook-body)
      (set-buffer-modified-p 'nil)   ; now kill-buffer won't complain!
      (kill-buffer "*Outlook Message*"))))

(provide 'outlookedit)
;;;end of file outlookedit.el
