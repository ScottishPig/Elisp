;
;                   E  L  I  S  P        ,6*"*VA. 
;                                       dN     V8 
;         pd*"*b.  ,pP""Yq.       ,AM   `MN.  ,g9 
;        (O)   j8 6W'    `Wb     AVMM    ,MMMMq.  
;            ,;j9 8M      M8   ,W' MM   6P   `YMb 
;         ,-='    YA.    ,A9 ,W'   MM   8b    `M9 
;        Ammmmmmm  'Ybmmd9'  AmmmmmMMmm `MmmmmM9  
;                                  MM             
;             Mark Burger, 2014    MM             
; 
; 1. Load this up with M-x eval-buffer OR put (load ".../2048.el") in your 
;       ~/.emacs file
; 2. Use M-x 2048-mode to start playing.
;
; Possible future additions: NxN board size rather than hard-coded 4x4
;


(setq 2048-plus-sep "+------+------+------+------+")
(setq 2048-space-sep "|      |      |      |      |")


(defun 2048-draw-board ()
"Draws the 4x4 gaming grid."
   	(dotimes (j 4)
   		(insert (concat 2048-plus-sep "\n") )
   		(insert (concat 2048-space-sep "\n") )
	   	(dotimes (i 4)
   			(let ( (k (2048-get-num-at i j)) (o "")  )
	   			(insert "|")
	   			(cond
	   				( (= k 0)  (setq o "      ")) 
	   				( (< k 10) (setq o (format "   %i  " k)))
	   				( (< k 100) (setq o (format "  %i  " k)))
	   				( (< k 1000) (setq o (format "  %i " k)))
	   				( t (setq o (format " %i " k))) )
				;(cond ((and (/= k 0) (2048-check-diff i j))
				;	(insert (propertize o 'face '(:foreground "green"))))
				;	(t (insert o)))
				(cond
					((= k 2) (insert (propertize o 'face '(:foreground "#888"))))
					((= k 4) (insert (propertize o 'face '(:foreground "#666"))))
					((= k 8) (insert (propertize o 'face '(:foreground "#FA3"))))
					((= k 16) (insert (propertize o 'face '(:foreground "#F99"))))
					((= k 32) (insert (propertize o 'face '(:foreground "#F66"))))
					((= k 64) (insert (propertize o 'face '(:foreground "#F00"))))
					((= k 128) (insert (propertize o 'face '(:foreground "#AA8"))))
					((= k 256) (insert (propertize o 'face '(:foreground "#CC6"))))
					((= k 512) (insert (propertize o 'face '(:foreground "#AA4"))))
					((= k 1024) (insert (propertize o 'face '(:foreground "#883"))))
					((= k 2048) (insert (propertize o 'face '(:foreground "#0A6"))))
					((= k 4096) (insert (propertize o 'face '(:foreground "#0F0"))))


					(t (insert o))
				)
	   		)
	   	)
	   	(insert (concat "|\n" 2048-space-sep "\n") )
	)
	(insert 2048-plus-sep)
)

(defun 2048-get-num-at (x y) "0, 0 is top-left" (aref (aref 2048-board y) x) )

(defun 2048-slide-single-piece (x y direction)
"Handles the movement of a single block at x,y in direction passed from
the 2048-shift function. Slides and merges for four directions."
	; THIS IS THE MOST TEDIUS THING FUCKING EVER.
	(catch 'breaker
		(when (2048-check-diff x y) (throw 'breaker 0))
		(let ( (i x) (j y) )
			(cond 
				; DONE. Blessed?
				((string= direction "r")
					(while (< i 3)
						(cond 
							; Next space is empty
							((= (2048-get-num-at (+ i 1) j) 0)
								(aset (aref 2048-board j) (+ i 1) 
									(2048-get-num-at i j))
								(aset (aref 2048-board j) i 0))
							; Next space is not current space (stop)
							((/= (2048-get-num-at i j) 
								(2048-get-num-at (+ i 1) j))
								(throw 'breaker 0))
							; Otherwise, squish the blocks
							(t (aset (aref 2048-board j) (+ i 1) 
								(* (2048-get-num-at i j) 2))
							   (aset (aref 2048-board j) i 0)
							   (throw 'breaker 0)
							) 
						)
						(setq i (+ i 1))
					)
				)
				; This 'un looks good, too.
				((string= direction "l")
					(while (> i 0)
						(cond 
							; Next space is empty
							((= (2048-get-num-at (- i 1) j) 0)
								(aset (aref 2048-board j) (- i 1) 
									(2048-get-num-at i j))
								(aset (aref 2048-board j) i 0))
							; Next space is not current space (stop)
							((/= (2048-get-num-at i j) 
								(2048-get-num-at (- i 1) j))
								(throw 'breaker 0))
							; Otherwise, squish the blocks
							(t (aset (aref 2048-board j) (- i 1) 
								(* (2048-get-num-at i j) 2))
							   (aset (aref 2048-board j) i 0)
							   (throw 'breaker 0)
							) 
						)
						(setq i (- i 1))
					)
				)
				; Up! Lookin' OK
				((string= direction "u")
					(while (> j 0)
						(cond 
							; Next space is empty
							((= (2048-get-num-at i (- j 1) ) 0)
								(aset (aref 2048-board (- j 1) ) i
									(2048-get-num-at i j))
								(aset (aref 2048-board j) i 0))
							; Next space is not current space (stop)
							((/= (2048-get-num-at i j) 
								(2048-get-num-at i (- j 1) ))
								(throw 'breaker 0))
							; Otherwise, squish the blocks
							(t (aset (aref 2048-board (- j 1)) i
								(* (2048-get-num-at i j) 2))
							   (aset (aref 2048-board j) i 0)
							   (throw 'breaker 0)
							) 
						)
						(setq j (- j 1))
					)
				)
				; Down! Test.
				((string= direction "d")
					(while (< j 3)
						(cond 
							; Next space is empty
							((= (2048-get-num-at i (+ j 1) ) 0)
								(aset (aref 2048-board (+ j 1) ) i
									(2048-get-num-at i j))
								(aset (aref 2048-board j) i 0))
							; Next space is not current space (stop)
							((/= (2048-get-num-at i j) 
								(2048-get-num-at i (+ j 1) ))
								(throw 'breaker 0))
							; Otherwise, squish the blocks
							(t (aset (aref 2048-board (+ j 1)) i
								(* (2048-get-num-at i j) 2))
							   (aset (aref 2048-board j) i 0)
							   (throw 'breaker 0)
							) 
						)
						(setq j (+ j 1))
					)
				)
			)
		)
			
	)
)

(defun 2048-count-empties ()
"Return the number of non-zero spaces in 2048-board array."
	(let ((k 0))
		(dotimes (j 4)
			(dotimes (i 4)
				(when (= (2048-get-num-at i j) 0) (setq k (+ k 1)))
			)
		)
		k
	)
)

(defun 2048-check-total-diff ()
"Run 2048-check-diff on the whole board. Return t/nil if board has changed
or not. Only relevant after 2048-push-board is called and modifications are
made to the 2048-board array."
	(catch 'breaker
		(dotimes (j 4)
			(dotimes (i 4)
				(when (2048-check-diff i j) (throw 'breaker t))
			)
		)
		(throw 'breaker nil)
	)
)

(defun 2048-check-diff (x y)
"Returns t if board has changed since beginning of shift."
	(/= (aref (aref 2048-board y) x)
		(aref (aref 2048-board-old y) x))
)

(defun 2048-push-board()
"Push 2048-board to 2048-board-old. Essentially makes a snapshot of the
game board to detect modifications later."
	(dotimes (j 4)
		(dotimes (i 4)
			(aset (aref 2048-board-old j) i (aref (aref 2048-board j) i))
		)
	)
)

(defun 2048-spawn ()
"Add either a 2 or 4 to the 2048-board array at a random, free space."
	(catch 'breaker
		(when (= (2048-count-empties) 0) (throw 'breaker 0))
		(let ((j (random 4)) (i (random 4)) (k (random 100)))
			(while (/= (2048-get-num-at i j) 0)
				(setq i (random 4))
				(setq j (random 4))
			)
			(cond ( (< k 75) (aset (aref 2048-board j) i 2 ))
				( t (aset (aref 2048-board j) i 4)))
		)
	)
)


(defun 2048-shift (direction)
"Entry point for shifting. All the keypresses get mapped to this function with
different arguments. This iterates across the board and slides the tiles
individually."
	(2048-push-board)
	(cond
		((or (string= direction "l") (string= direction "u"))
			(dotimes (j 4)
				(dotimes (i 4)
					(2048-slide-single-piece i j direction)
				)
			)
		)

		((or (string= direction "r") (string= direction "d"))
			(dotimes (j 4)
				(dotimes (i 4)
					(2048-slide-single-piece (- 3 i) (- 3 j) direction)
				)
			)
		)
	)

	(when 
		(2048-check-total-diff) (2048-spawn))
		 

	; Redraw... even if stuff don't need it.
	(erase-buffer)
	(2048-draw-board)


)

(defun 2048-shift-left () (interactive) (2048-shift "l")  ) 
(defun 2048-shift-right () (interactive) (2048-shift "r"))
(defun 2048-shift-up () (interactive) (2048-shift "u"))
(defun 2048-shift-down () (interactive) (2048-shift "d"))

(defun 2048-mode ()
"Starts up the game mode for 2048!"
	(interactive)

	(random t) ; Start up the RNG
	(setq 2048-board (make-vector 4 0))
	(dotimes (i 4) (aset 2048-board i (make-vector 4 0)) )

	(setq 2048-board-old (make-vector 4 0))
	(dotimes (i 4) (aset 2048-board-old i (make-vector 4 0)) )

	(dotimes (i 2) (2048-spawn))

	(switch-to-buffer "2048")
	(local-set-key (kbd "<left>") '2048-shift-left)
	(local-set-key (kbd "<right>") '2048-shift-right)
	(local-set-key (kbd "<down>") '2048-shift-down)
	(local-set-key (kbd "<up>") '2048-shift-up)

	(2048-draw-board)
)


