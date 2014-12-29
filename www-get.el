; Mark Burger, 8/8/12
;
; Put the awesome power of wget into emacs.
;
; (www-get "ads.mp.mydas.mobi/...") C-x C-e
; or M-x www-get
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


	(goto-char 0) (while (re-search-forward "<a.*? href=\"\\(.*?\\)\".*?>" nil t)
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










 
