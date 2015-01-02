; ZENDESK MODE
; DECEMBER 2013 / JANUARY 2014 - MARK BURGER
; EDITS: MARCH 2014

;
; S E T U P
; + Ensure you have curl w/ HTTPS support
; + Log in to Zendesk via browser. Click on your view for new tickets and copy
;      the view number from the address bar ( ...zendesk.com/agent/#/filters/27820767 --> 27820767 ).
; + Put this number in the zd-mode function below: i.e. (setq zd-new-view "27820767")
; + Click on the view in your browser again, this time for your assigned tickets
;	  (...zendesk.com/agent/#/filters/25370986 --> 25370986)
; + Put this number in the zd-mode function below: i.e.
;     (setq zd-assigned-view "25370986")
; + To prevent the annoying re-typing of user username for each zd-mode launch,
;     update the zd-un line in the zd-mode function: i.e.
; 	  ;(setq zd-un (read-from-minibuffer "Username: "))	
;	  (setq zd-un "yourface@yourcompany.com" )
;
; Adding new views to the main ZD-mode screen:
; inside zd-build-main-view, add a heading and a call to (zd-insert-basic-list ) with some 
; view ID number.
;

;
; R U N N I N G:
; Probably set a key (C-x >) to start zd-mode.
; C-f - find a ticket
; C-S-f - Look up a ticket by number
; < - Return to main from ticket
; ! - Launch browser from ticket
;





(defun zd-call-curl (url)
	"Call curl (used to use wget) for 'url' and output to zd-temp-buffer"

	; This used to say "generate-new-buffer", but this way, repeated calls to wget for paginated
	; data can be scraped by sharing a buffer, rather than throwing the baby out with the bathwater.
	(call-process curl-command nil
		zd-temp-buffer t
		"-s" ; The 's' is for "shhh!"
		"-A" "Mozilla/5.0 (X11; Linux x86_64; rv:10.0.1) Gecko/20100101 Firefox/10.0.1"
		"-H" (concat "Authorization: Basic " (base64-encode-string (concat zd-un ":" zd-pw))  ) ; Insecure
		url
	)
	(switch-to-buffer zd-temp-buffer)

)


(defun zd-call-curl-for-put (url putdata)
	"Call curl (used to use wget) for 'url' and output to zd-temp-buffer"

	(call-process curl-command nil
		zd-temp-buffer t
		"-s" ; The 's' is for "shhh!"
		"-A" "Mozilla/5.0 (X11; Linux x86_64; rv:10.0.1) Gecko/20100101 Firefox/10.0.1"
		"-H" (concat "Authorization: Basic " (base64-encode-string (concat zd-un ":" zd-pw))  )
		"-H" "Content-Type: application/json"
		"-d" putdata
		"-X" "PUT" ; <- This is the reason for dropping wget in favor of curl. :-(
		url
	)
	(switch-to-buffer zd-temp-buffer)

)


(defun zd-recalc-buf-name (change-to)
	(cond 
		( (or (string= change-to "open") (string= change-to "new") (string= change-to "pending") (string= change-to "solved"))
			(replace-regexp-in-string "\\(new\\|open\\|pending\\|solved\\)" change-to (buffer-name))
		)

		( (or (string= change-to "public") (string= change-to "private") )
			(replace-regexp-in-string "\\(public\\|private\\)" change-to (buffer-name))
		)
	)
)


; http://developer.zendesk.com/documentation/rest_api/tickets.html
(defun zd-put-reply ()
    "Must be called from a ZD-reply buffer!"

	(setq zd-json "{\"ticket\":{\"subject\": \"")

	(goto-char 0)
		(re-search-forward "Subject: \\(.*\\)\n")
	(goto-char 0)
	(setq zd-json (concat zd-json (match-string 1) "\", \"status\": \"" ))	


	;    0    1      2    3
	; public-reply-open-17625
	(setq zd-tickdata (split-string (buffer-name) "-"))

	(setq zd-json (concat zd-json 
		(nth 2 zd-tickdata) "\", \"comment\": { \"public\": "))

	(setq zd-public "false")
	(when (string= (nth 0 zd-tickdata) "public") (setq zd-public "true"))
	(setq zd-json (concat zd-json zd-public ", \"body\": \""  ))


	(string-match "Subject: [^\n]*\n\n" (buffer-string))
	(setq zd-tickbody (buffer-substring (match-end 0) (point-max)))

	;; THIS PART WORKS NOW-- should just went with (with-temp-buffer...)
	(setq zd-tickbody
		(replace-regexp-in-string "\"" "\\\\\"" 
			(replace-regexp-in-string "\n" "\\\\r\\\\n" (substring zd-tickbody 1))
		)
	)
	; ^^^ BLAYUM; YOU'RE UGLY. CONSIDER with-temp-buffer INSTEAD?

	;; TODO: ESCAPE TICKET RESPONSE!
	(setq zd-json (concat zd-json zd-tickbody "\" } } }"))  


	; Beep-boop. Testing!
	; (message (concat zd-url-ticket-prefix (nth 3 zd-tickdata) ".json"))
	; (message zd-json)

	(zd-call-curl-for-put
		(concat zd-url-ticket-prefix (nth 3 zd-tickdata) ".json")
		zd-json
	)


)

(defun zd-perform-reply () 
	"Must be called from a ticket buffer (filled with the funky near-JSON)!"
	(interactive)

	(goto-char 0)(re-search-forward "id: \\([^,]*\\),")(setq zd-ticket-id (match-string 1))
	(goto-char 0)(re-search-forward "subject: \\(.*\\),")(setq zd-ticket-subject (match-string 1))
	(goto-char 0)(re-search-forward "\npriority: \\(.*\\),")(setq zd-ticket-priority (match-string 1))
	(setq zd-ticket-status "open")

	(switch-to-buffer (concat "public-reply-" zd-ticket-status "-" zd-ticket-id))

	;; All ticket-related data should be either in the buffer body or in the buffer itself since the variables
	;; above will change if the user opens another ticket for reply.


	(insert "Visibility: ")
	(insert-button "Private" 'action (lambda (a) (rename-buffer (zd-recalc-buf-name "private")) ))(insert " ")
	(insert-button "Public" 'action (lambda (a) (rename-buffer (zd-recalc-buf-name "public")) ))

	(insert "\nStatus: ")
	(insert-button "New" 'action (lambda (a) (rename-buffer (zd-recalc-buf-name "new")) ))(insert " ")
	(insert-button "Open" 'action (lambda (a) (rename-buffer (zd-recalc-buf-name "open")) ))(insert " ")
	(insert-button "Pending" 'action (lambda (a) (rename-buffer (zd-recalc-buf-name "pending")) ))(insert " ")
	(insert-button "Solved" 'action (lambda (a) (rename-buffer (zd-recalc-buf-name "solved")) ))
	(insert "\n")
	(insert-button "Submit" 'action (lambda (a) (zd-put-reply) ))
	(insert "\n\n")


	(insert (concat "\nSubject: " zd-ticket-subject "\n\n"))

	(goto-char 0)

)


(defun zd-insert-basic-list (view) 
	(setq ids (list))
	(setq subjects (list))

	(zd-call-curl (concat zd-url-view-prefix view zd-url-view-suffix) )
	; (message (concat zd-url-view-prefix view zd-url-view-suffix) )


	; This one works with truncated subjects... don't touch.
	;(while (re-search-forward  "\"ticket\":{\"id\":\\([0-9]*\\),\"subject\":\"\\([^\"]*\\)"  nil t)

	(goto-char 0)
	(while (re-search-forward  "\"subject\":\"\\([^\"]*\\)[^\\}]*\\}[^\\}]*\"ticket\":{\"id\":\\([0-9]*\\)"  nil t)
		(add-to-list 'subjects (match-string 1))
		(add-to-list 'ids (match-string 2))
	)

	(kill-buffer zd-temp-buffer)

	(insert (concat "Total: " (number-to-string (length ids)) "\n"))

	(setq i 0)
	(while (< i (length ids))
		(insert-button (concat (nth i ids) " -> " (nth i subjects)) 
			'action (lambda (a) (zd-inspect-ticket (nth 0 (split-string (button-label (button-at (point)))))) )
		)

		(insert "\n")
		(setq i (+ i 1))
	)

)

(defun zd-insert-search-list (searchterm)
	"Search for tickets"
	(setq ids (list))
	(setq subjects (list))
	
	(setq i 1)
	(while (< i 5)
		(zd-call-curl (concat zd-url-search-prefix searchterm "&by_updated=1&page=" (number-to-string i)) )
		(setq i (+ i 1))
	)


	; THIS IS MONDO-UGLY!
	; "id":16356,"created_at":"2013-12-06T06:42:14Z","updated_at":"2013-12-26T14:24:11Z","type":"Incident","subject":"

	(goto-char 0)
	(while (re-search-forward  "\"id\":\\([0-9]*\\)[^s]*\"subject\":\"\\([^\"]*\\)"  nil t)
		(add-to-list 'ids (match-string 1))
		(add-to-list 'subjects (match-string 2))
	)

	(kill-buffer zd-temp-buffer)

	(goto-char (point-max))
	(insert (concat "\n\nSearch results for \"" searchterm "\":\n"))
	(setq i 0)
	(while (< i (length ids))
		(insert-button (concat (nth i ids) " -> " (nth i subjects)) 
			'action (lambda (a) (zd-inspect-ticket (nth 0 (split-string (button-label (button-at (point)))))) )
		)
		(insert "\n")
		(setq i (+ i 1))
	)

)


(defun zd-build-main-view ()
	"Make the main view with tickets assigned to you and unassigned."
	(interactive)
	(switch-to-buffer zd-main-buffer)
	(erase-buffer)

	(insert (propertize "New tickets\n" 'face '(:height 200 :family "Arial" ) )  )
	(zd-insert-basic-list zd-new-view)

	(insert (propertize "\nAssigned tickets\n" 'face '(:height 200 :family "Arial" ) )  )
	(zd-insert-basic-list zd-assigned-view)

	; Add more queues here, if you like...

	(goto-char 0)
)

(defun zd-perform-lookup ()
	"Front-end to zd-inspect-ticket"
	(interactive)
	(zd-inspect-ticket (read-from-minibuffer "Ticket number: "))
)

(defun zd-perform-search ()
	"Front-end to insert-search-list"
	(interactive)
	(let ((query (read-from-minibuffer "Term: ")))
		(zd-insert-search-list query)
	)
)

(defun zd-inspect-ticket (whichone) 
	"Open a new buffer and pretty-print some ticket data."
	(interactive whichone)

	; BASE TICKET DATA - temporary since we're seeing blank tickets
	(zd-call-curl (concat zd-url-ticket-prefix whichone ".json" ))
	(setq zd-baseticket (buffer-string))

	(zd-call-curl (concat zd-url-ticket-prefix whichone "/audits.json?include=users" ))


	; Build users table:
	(setq zd-user-ids (list))
	(setq zd-user-names (list))
	(setq zd-user-emails (list))
	
	(goto-char 0)

	(search-forward "users\":[{" )
	(while (re-search-forward  "\"id\":\\([0-9]*\\),[^,]*,\"name\":\"\\([^\"]*\\)\",\"email\":\"\\([^\"]*\\)" nil t)
		(add-to-list 'zd-user-ids (match-string 1))
		(add-to-list 'zd-user-names (match-string 2))
		(add-to-list 'zd-user-names (match-string 3))
		; (message (concat "Located: " (match-string 1) " -> " (match-string 2) " -> " (match-string 3) ))
	)

	; Blank ticket bug...
	(goto-char 0)(insert (concat zd-baseticket "\n\n"))

	; Niceties...
	(goto-char 0) (while (search-forward "\\n" nil t) (replace-match "\n"))
	(goto-char 0) (while (search-forward "\\r" nil t) (replace-match ""))
	(goto-char 0) (while (search-forward "<br />" nil t) (replace-match "\n"))
	(goto-char 0) (while (search-forward "<p>" nil t) (replace-match "\n"))
	(goto-char 0) (while (search-forward "</p>" nil t) (replace-match "\n"))

	(goto-char 0) (while (re-search-forward "\"\\([a-z0-9_]+\\)\":" nil t) (replace-match 
		(concat "\n" (propertize (match-string 1) 'face zd-json-properties) ": " )) )

	(dotimes (i (length zd-user-ids))
			;(message (concat "Replacing (" (number-to-string i) "): " (nth i zd-user-ids) " -> " (nth i zd-user-names)))
			(goto-char 0) (while (search-forward (nth i zd-user-ids) nil t) (replace-match (nth i zd-user-names)) )
	)

	; Final niceties:
	(goto-char 0) (while (search-forward "\\\"" nil t) (replace-match "#ZDQUOTE#"))
	(goto-char 0) (while (search-forward "\"" nil t) (replace-match ""))
	(goto-char 0) (while (search-forward "#ZDQUOTE#" nil t) (replace-match "\""))

	(goto-char 0)
	(longlines-mode)
	(rename-buffer whichone)
	(local-set-key (kbd "C-r") 'zd-perform-reply)
	(local-set-key (kbd "!") 'zd-launch-url)
	(local-set-key (kbd "<")  'zd-build-main-view )	

	(goto-char 0)
)


(defun zd-launch-url ()
	(interactive)
	(browse-url (concat "https://" zd-domain "/agent/#/tickets/" (buffer-name)))
)

(defun zd-mode ()
	"Start Zendesk Mode"

	(interactive)

	;(setq curl-command "/usr/pkg/bin/curl") ; SDF
	(setq curl-command "/usr/bin/curl" ) ; Work
	;(setq curl-command "d:\\git\\bin\\curl") ; Home-Vista

	(setq zd-domain "yourcompany.zendesk.com")


	(setq zd-un (read-from-minibuffer "Username: "))	
	;(setq zd-un "you@yourcompany.com" )
	(setq zd-pw (read-passwd (concat "Password for " zd-un ": ") ))

	; JSON header colors
	(setq zd-json-properties '(:foreground "#9F0") )

	; Modify these!
	(setq zd-assigned-view "25370986")
	(setq zd-new-view "27820767")


	(setq zd-url-view-prefix (concat "https://" zd-domain "/api/v2/views/" ))
	(setq zd-url-view-suffix "/execute.json?per_page=60&page=1" )

	(setq zd-url-search-prefix (concat "https://" zd-domain "/api/v2/search/incremental?query="))

	(setq zd-url-ticket-prefix (concat "https://" zd-domain "/api/v2/tickets/"))

	(setq zd-temp-buffer "zdtemp")
	(setq zd-main-buffer "Zendesk")

	(zd-build-main-view)

	(local-set-key (kbd "C-f")  'zd-perform-search )
	(local-set-key (kbd "C-S-f") 'zd-perform-lookup )
	(local-set-key (kbd "<")  'zd-build-main-view )

)

; (message "Ok, glass.")
; (zd-build-main-view)
