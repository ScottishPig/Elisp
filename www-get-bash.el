; Open up some BASH when you're feeling blue. Requires
; wget... but you can modify the call-process to use
; curl, if you're a loser.
;
; Mark Burger, December 29, 2014

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
