; Mark Burger, 8/8/12
; Updated / compiled 1/14/2015
;
; Put the awesome power of wget into emacs.
;
; www-get-parse tries to strip away some ugly
; HTML markup, but doesn't do a particularly
; good job.
;
; Requires wget ( http://www.gnu.org/software/wget/ )
; and emacs. ( http://www.gnu.org/software/emacs/ )
;
; If you'd prefer use curl, change call-process'
; arguments accordingly. I don't care.

; ADDED: Hipchat stuff. Nicey functions for quick launching
; google image searches, bash quotes, memecaptain stuff.

; MEMECAPTAIN uses bitly to pretty up the links. Requires an API
; key. It's free. Just do it. In your .emacs file, set up
; (setq bitly-token "...")

(defun www-get (&optional url)
	"Get, but don't parse, an HTTP request."
	(interactive)
	(unless url
	(setq url (read-from-minibuffer "URL: ") )
	)
	(call-process "wget" nil t t "-qO" "-" url )
)

(defun www-get-parse (&optional url)
	"Get and hack at an HTML document."
	(interactive)
	(if url (www-get url) (www-get) )


	(goto-char 0) (while (re-search-forward "<a.*? href=\"\\(.*?\\)\".*?>" 
		nil t)
		(replace-match "[\\1]" nil nil))

	(goto-char 0) (while (re-search-forward "<a.*?>" nil t)
		(replace-match "\n" nil nil))

	(goto-char 0) (while (search-forward "&gt;" nil t)
		(replace-match ">" nil nil))

	(goto-char 0) (while (search-forward "&lt;" nil t)
		(replace-match "<" nil nil))

	(goto-char 0) (while (search-forward "&quot;" nil t)
		(replace-match "\"" nil nil))

	(goto-char 0) (while (search-forward "" nil t)
		(replace-match "" nil nil))

	(goto-char 0) (while (search-forward "&nbsp;" nil t)
		(replace-match " " nil nil))

	(goto-char 0) (while (search-forward "\n\n" nil t)
		(replace-match "\n" nil nil))

	(goto-char 0) (while (search-forward "\t" nil t)
		(replace-match " " nil nil))

	(goto-char 0) (while (re-search-forward "\s+" nil t)
		(replace-match " " nil nil))


)


(defun www-get-bash ()
"Open a new buffer and read some neatly-formatted bash.org"

	(interactive)
	(switch-to-buffer "bash-scratch")

	;(www-get "http://bash.org/?random1")
	(call-process "wget" nil t t "-qO" "-" "http://bash.org/?random1" )

	(goto-char 0)

	(setq i 0 j 0)

	(while (and i j)
		; Rip out each quote and process 'em one at a time.
		(switch-to-buffer "bash-scratch")
		(setq i (search-forward "<p class=\"quote\">" nil t))
		(setq j (search-forward "</p>\n" nil t))
	
		(when (and i j)
			(setq data (buffer-substring i j))
			(with-temp-buffer 
				(insert data)

				(goto-char 0) (while (re-search-forward "<a [^>]*>" nil t)
					(replace-match "" nil nil))

				(goto-char 0) (while (re-search-forward 
						"</b></a>.*</p><p class=\"qt\">" nil t)
					(replace-match "\n" nil nil))

				(goto-char 0) (while (search-forward "<b>" nil t)
					(replace-match "" nil nil))
				(goto-char 0) (while (search-forward "" nil t)
					(replace-match "" nil nil))
				(goto-char 0) (while (search-forward "<br />" nil t)
					(replace-match "" nil nil))
				(goto-char 0) (while (search-forward "</p>" nil t)
					(replace-match "" nil nil))

				(goto-char 0) (while (search-forward "&quot;" nil t)
					(replace-match "\"" nil nil))
				(goto-char 0) (while (search-forward "&gt;" nil t)
					(replace-match ">" nil nil))
				(goto-char 0) (while (search-forward "&lt;" nil t)
					(replace-match "<" nil nil))
				(goto-char 0) (while (search-forward "&nbsp;" nil t)
					(replace-match " " nil nil))

				(setq data (buffer-string))
			)
			(switch-to-buffer "bash")
			(insert (concat data "\n\n"))
		)
	
		
	)
	
	(kill-buffer "bash-scratch")
	(switch-to-buffer "bash")
	(goto-char 0)
	(longlines-mode)
)


(defun uri-escape (url) 
	(let ((retval ""))
		(dotimes (i (length url))
			(cond 
				( (or (and (>= (aref url i) 48) (<= (aref url i) 57))
						(and (>= (aref url i) 65 ) (<= (aref url i) 90))
						(and (>= (aref url i) 97) (<= (aref url i) 122)))
					(setq retval (concat retval (char-to-string (aref url i))))
				)
				(t (setq retval (concat retval "%" (format "%X" (aref url i)))))
			)
		)
		retval
	)
)

; MEMECAPTAIN GOTOS--

(defun memecaptain(&optional returnString)
"Prompt for image URL, top-text, bottom-text. If called with returnString
not nil, it can be used in a script and quietly return a value. Otherwise, 
it'll open a browser (and still return the memecaptain URL)."
	(interactive)
	(let ((q ; Very first nested 'let'! EMACS POWER!
		(let (
			(img (read-from-minibuffer "Image: "))
			(top (read-from-minibuffer "Top text: "))
			(bottom (read-from-minibuffer "Bottom text: " )))
			(concat "http://v1.memecaptain.com/i?"
				"u=" (uri-escape img) 
				"&t1=" (uri-escape top) 
				"&t2=" (uri-escape bottom))))
		(bq ""))
		; Do three things: Browse to it for preview, return it for scripts
		; and move it to the kill ring for maximum paste/sharetimes.
		(setq bq (bitly q))
		(with-temp-buffer
			(insert bq)
			(kill-ring-save 1 (point-max))
		)
		(when (not returnString) (browse-url bq))
		bq
	)
)


(defun bitly(url)
	(unless url
		(setq url (read-from-minibuffer "URL: "))
	)

	(ignore-errors (kill-buffer "bitly-temp"))

	(call-process "curl" nil "bitly-temp" nil
	   	"-s"
	   	(concat "https://api-ssl.bitly.com/v3/shorten?"
	   		"format=txt"
			"&access_token=" bitly-token
			"&longUrl=" (uri-escape url)))
	(let ((retval ""))
		(switch-to-buffer "bitly-temp")
		(setq retval (buffer-string))
		(kill-buffer "bitly-temp")
		(substring retval 0 (- (length retval) 1)) ; remove newline
	)
)





; GOOGLE GOTOS---

(defun googim ()
"Prompt for search term, then launch browser and Google Image search it."
	(interactive)
	(let ((input (read-from-minibuffer "Term: ")))
		(browse-url (concat "https://www.google.com/search?tbm=isch&q=" input))
	)
)

(defun googim-safe ()
"Prompt for search term, then launch browser and Google Image safe-search it."
	(interactive)
	(let ((input (read-from-minibuffer "Term: ")))
		(browse-url (concat "https://www.google.com/search?tbm=isch&safe=active&q=" input))
	)
)

(defun googim-animate ()
"Prompt for search term, then launch browser and Google Image search for animated images of it."
	(interactive)
	(let ((input (read-from-minibuffer "Term: ")))
		(browse-url (concat "https://www.google.com/search?tbm=isch&tbs=itp:animated&q=" input))
	)
)

(defun googim-safe-animate ()
"Prompt for search term, then launch browser and Google Image safe-search for animated images of it."
	(interactive)
	(let ((input (read-from-minibuffer "Term: ")))
		(browse-url (concat "https://www.google.com/search?tbm=isch&tbs=itp:animated&q=" input))
	)
)








 
