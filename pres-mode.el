; PRES-MODE for Emacs... Tool for really minimalistic presentations
; 2014, Mark Burger
; For details on usage, use C-h f pres-mode

(defun pres-output() 
	(let ( (fmt (downcase (aref (nth pres-i pres-presentation) 0)))
			(txt (aref (nth pres-i pres-presentation) 1))
			(func nil)
			(r 0)
		 )
		
		; When first char in the fmt elemnt is "c", clear the buffer...
		(when (> (length fmt) 0)
			(when (string= (substring fmt 0 1) "c") (erase-buffer) )
		)

		(with-temp-buffer
			(insert fmt)
			(goto-char 0)
			(setq r (re-search-forward ".*h\\(.\\)" nil t))

			(cond 
				( (not r) (setq func (lambda () (insert (propertize 
					(concat "*   " txt) 'face '(:height 180)))) ) )
				( (string= (match-string 1) "1") (setq func (lambda () 
					(insert (propertize txt 'face 
						'(:height 300 :family "Arial" )))) ) )
				( (string= (match-string 1) "2") (setq func (lambda () 
					(insert (propertize txt 'face 
						'(:height 260 :family "Arial" )))) ) )
			)


		)
		; Get outta the temp buffer and do our business...
		(when (functionp func) (funcall func))
		(insert "\n")
	)
)

(defun pres-advance ()
"Increment the presentation index, and present from pres-presentation list. 
Usually defined as SPACE."
	(interactive)
	(message (concat (number-to-string pres-i ) " / " 
		(number-to-string (- (length pres-presentation) 1))) )

	(cond
		( (< pres-i (length pres-presentation)) 
			(progn (pres-output)(setq pres-i (+ 1 pres-i))) )
		( t (message "End.") )
	)
)


(defun pres-mode ()
"Start up pres-mode. Requires a data structure defined 
called pres-presentation where the first element of 
pres-presentation is a string and the remaining elements 
are arrays. These arrays should have two elements-- each 
strings. First element is the format, and the second element 
is the content. If the format begins with a 'c', the screen 
is cleared. If it contains h1 or h2, a heading face is used, 
if supported. When running, press SPACE to advance presentation.
An example presentation would look like this--

\(setq pres-presentation 
	(list
		\"Example Presentation\"
		[\"ch1\" \"Example\"]
		[\"h2\" \"Ex2\"]
		[\"\" \"Ex3\"]
		[\"c\" \"Ex4\"]
	)
)
\(pres-mode)
"

	(interactive)
	(setq pres-i 1)
	(cond
		( (not (boundp 'pres-presentation)) 
			(message (concat "List pres-presentation missing... " 
				"see C-h f pres-mode for details.")) )
		( t (progn (switch-to-buffer (nth 0 pres-presentation) ) 
			(local-set-key (kbd "SPC") 'pres-advance) ) )
	)

	
)


