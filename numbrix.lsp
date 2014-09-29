(defparameter flag (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		))

(defun numbrix()
	(setq flag (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
			 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		))
	(terpri)(princ "RULES OF THE GAME:")
	(terpri)(princ "1.For N*N board, fill the board with sequence of consecutive numbers from 1 to N^2.")
	(terpri)(princ "2.The numbers must follow a horizontal or vertical path (no diagonals).")
	(terpri)(princ "3.To start, go on empty square, and enter the correct number.")
	
	(terpri) (terpri) (princ "SELECT THE BOARD NUMBER FROM 1 TO 17") (terpri)
	(princ "Option: ")
	(setq i (read))
	(terpri) (princ "FOR MANUAL PLAYING PRESS 1 / FOR AUTO-PLAY PRESS 2 : ")
	(setq type (read))
	(terpri) (princ "*****START GAME*****") (terpri)
	(cond ((equal i 1) (setq brd1 (list '(1 -- -- -- 9) 
					    '(-- -- 6 -- --) 
					    '(-- 18 -- 14 --) 
					    '(-- -- 16 -- --) 
					    '(21 -- -- -- 25) ) )
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 1 1 1 1 0 1 1 0 1 1 1 1 0 1 0 1 0 1 1 0 1 1 1 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )

	     ((equal i 2) (setq brd1 (list '(3 4 9 10 15 16) 
					   '(2 -- -- -- -- 17) 
					   '(1 -- -- -- -- 18) 
					   '(36 -- -- -- -- 19)
					   '(31 -- -- -- -- 24) 
  					   '(30 29 28 27 26 25) ) )
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 0 0 0 1 1 1 1 0 0 1 1 1 1 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0)) 
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )

	     ((equal i 3) (setq brd1 (list '(1 -- 9 -- 25 -- 49) 
					   '(-- 3 -- 11 -- 27 --) 
					   '(5 -- -- -- -- -- 47) 
					   '(-- 15 -- -- -- 29 --) 
					   '(17 -- -- -- -- -- 45) 
			  		   '(-- 35 -- 33 -- 31 --) 
					   '(37 -- 39 -- 41 -- 43) ) )
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 1 0 1 0 1 1 1 0 1 0 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )
	   
	    ((equal i 4) (setq brd1 (list '(54 -- 52 -- -- 37 -- 35) 
					  '(-- -- -- -- -- -- -- --) 
					  '(58 -- 48 -- -- 41 -- 31) 
					  '(-- -- -- 46 -- -- -- --) 
				          '(-- -- -- -- 16 -- -- --) 
				          '(63 -- 5 -- -- 18 -- 26) 
			   		  '(-- -- -- -- -- -- -- --) 
			 		  '(1 -- 11 -- -- 20 -- 22)))
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 1 1 1 0 1 1 1 1 1 0 1 1 1 1 0 1 0 1 0 1 0 1 1 1 0 1 1 1 1 0 1 1 1 0 1 0 1 1 1 0 1 1 1 1 0 1 0 1 1 1 0 1 0 1 1 1 0 1 1 1 1 0 1))
				(setq avail (make_list1 available)) 
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )

	   ((equal i 5) (setq brd1 (list '(1 -- -- -- 9) 
					 '(-- -- -- -- --) 
					 '(-- -- 21 -- --) 
					 '(-- -- -- -- --) 
			     		 '(25 -- -- -- 15) ) )
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 1 1 1 1 
			 			      1 1 1 0 1 
			 			      1 1 1 1 0 
			 			      1 1 1 1 1 
		   	 			      0 1 1 1 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )

	  ((equal i 6) (setq brd1 (list '(25 -- -- -- 13) 
					'(-- -- -- -- --) 
					'(-- -- 1 -- --) 
					'(-- -- -- -- --) 
					'(21 -- -- -- 17) ) )
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 1 1 1 1
			 			      1 1 1 1 1 
			 			      1 1 0 1 1 
			 			      1 0 1 1 1 
			 			      0 1 1 1 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )

	((equal i 7) (setq brd1 (list '(49 -- -- -- -- -- 31) 
				      '(-- 9 -- 3 -- -- --) 
				      '(-- -- -- -- -- -- --) 
				      '(-- -- -- 5 -- -- --) 
				      '(-- -- -- -- 17 -- --) 
				      '(-- -- -- -- -- 25 --) 
				      '(-- -- -- -- -- -- 37) ) )
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 1 0 1 0 1 1 
			 			      1 0 1 1 1 1 1 
			 			      1 1 0 1 1 1 1 
			 			      1 1 1 0 1 1 1 
						      1 1 0 1 1 1 1 
						      1 0 1 1 1 1 1 
						      1 1 1 1 1 1 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (set_value1 5 3 1 new_list) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )	
	
	((equal i 8) (setq brd1 (list '(1 -- -- -- -- -- -- 64) 
				      '(-- 7 -- -- -- -- 38 --) 
				      '(-- -- -- 14 19 -- -- --) 
				      '(-- -- -- -- -- -- -- --) 
				      '(-- -- -- -- -- -- -- --) 
				      '(-- -- -- 29 30 -- -- --) 
				      '(-- 48 -- -- -- -- 43 --) 
				      '(50 -- -- -- -- -- -- 57)))
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 1 1 1 1 1 0 1
			 			      1 1 1 1 1 0 1 1 
						      1 1 0 1 1 1 1 1 
						      1 1 1 1 0 0 1 1 
						      1 1 1 1 1 0 1 1 
			 			      1 1 0 1 1 1 1 0
			 			      1 0 1 1 1 1 1 1 
						      0 1 1 1 1 1 1 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )	

	((equal i 9) (setq brd1 (list '(-- -- -- -- -- -- -- -- --) 
				      '(-- 75 -- 9 -- 3 -- 43 --) 
 				      '(-- -- 79 -- 1 -- 15 -- --) 
				      '(-- 77 -- -- -- -- -- 41 --) 
				      '(-- -- 21 -- -- -- 17 -- --) 
				      '(-- 67 -- -- -- -- -- 39 --) 
				      '(-- -- 31 -- 29 -- 27 -- --) 
				      '(-- 63 -- 33 -- 35 -- 37 --) 
				      '(-- -- -- -- -- -- -- -- --)))
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 1 0 1 1 1 1 1 0
						      1 1 1 1 1 0 1 0 1 
						      1 1 0 1 1 1 1 1 0
			 			      1 0 1 0 1 0 1 0 1
			 			      0 1 0 1 0 1 0 1 1
			 			      1 1 1 1 1 1 1 1 1 
						      1 1 1 1 1 1 1 1 0
						      1 1 1 0 1 1 1 1 1 
						      1 1 0 1 0 1 0 1 1))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )

	((equal i 10) (setq brd1 (list '(73 -- 81 -- 11 -- 13 -- 45) 
					'(-- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- 1 -- -- -- --) 
					'(-- -- -- -- -- -- -- -- --) 
					'(-- 68 -- -- 19 -- -- 40 --) 
					'(-- -- -- -- -- -- 26 -- --) 
					'(-- -- -- -- 29 -- -- -- --) 
					'(-- -- 32 -- -- -- 36 -- --) 
					'(61 -- -- -- -- -- -- -- 53)))
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 1 1 1 1 1 1 1 1 1 
						      0 1 0 1 1 1 1 1 0 1
						      1 1 1 1 1 0 1 1 0 1
						      1 0 1 1 1 0 1 1 1 0
						      1 1 1 1 0 1 1 1 1 1
						      1 1 0 1 1 1 1 1 1 1
						      0 1 1 1 1 1 1 0 1 1
						      1 1 0 1 1 1 1 1 1 1 0))
				(setq avail (make_list1 available))	
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )

	((equal i 11) (setq brd1 (list '(81 -- 79 -- -- -- -- 74 -- 72) 
				 	'(-- -- -- 15 -- -- 18 -- -- --) 
					'(-- -- -- -- -- -- -- -- -- --) 		
					'(-- -- -- -- -- -- -- -- -- --) 
					'(-- -- 91 -- 1 -- -- -- -- 64)
					'(-- -- 32 -- -- 9 -- -- -- --)
				        '(-- -- -- -- -- -- 25 -- -- --) 
					'(-- -- -- -- -- -- -- 47 -- --) 
					'(-- -- -- -- -- -- -- -- 53 -- ) 
					'(98 -- 38 -- -- -- -- -- -- 59)))
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 1 1 1 1 1 1 1 0 1
			  			      1 1 1 1 0 1 1 0 1 1
             		  			      1 1 1 1 0 1 1 1 1 1 
 			 			      1 0 1 1 1 1 1 0 1 1
			 			      1 1 1 1 1 1 0 1 1 1
			  			      1 1 0 1 1 1 1 1 0 1
			  			      1 1 1 0 1 1 1 1 1 1 
			 			      1 0 1 0 1 1 1 1 0 1 
			  			      0 1 1 1 1 1 1 1 1 1 
			  			      0 1 1 1 1 1 1 0 1 1))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )

	((equal i 12) (setq brd1 (list '(68 -- -- -- -- -- -- -- -- 77) 
					'(-- 50 -- -- -- -- -- -- -- --) 
					'(-- -- 20 -- -- -- -- 29 -- --) 
					'(-- -- -- 22 -- -- -- -- -- --) 
					'(-- -- -- -- 10 -- -- -- -- --) 
					'(-- -- -- -- 1 6 -- -- -- --) 
					'(-- -- -- -- -- -- 34 -- -- --) 
					'(-- -- 15 -- -- -- -- 36 -- --) 
					'(-- -- -- -- -- -- -- -- 86 -- ) 
					'(59 -- -- -- -- -- -- -- -- 100)))
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list   0 1 1 1 1 0 1 1 1 0
							1 1 1 1 0 1 1 1 1 0
							1 0 1 1 1 1 1 1 0 1
							1 1 1 0 1 0 1 1 1 1
							1 1 1 1 1 1 1 1 1 0
							1 1 1 1 1 1 1 1 0 1
							1 1 1 1 1 1 1 0 1 1
							1 1 1 1 1 1 0 1 1 1
							1 1 1 1 1 0 1 1 1 1
							1 1 1 1 1 1 1 1 1 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )		

	((equal i 13) (setq brd1 (list '(1 4 5 6 7 8 21 22 41 42 43) 
					'(2 -- -- -- -- -- -- -- -- -- 44) 
					'(119 -- 13 -- -- -- -- -- -- -- 47) 
					'(120 -- -- -- 104 -- -- -- -- -- 48) 
					'(121 -- -- -- -- -- -- -- -- -- 51) 
					'(112 -- -- -- -- -- -- -- -- -- 52) 
					'(111 -- -- -- -- -- -- -- 35 -- 55) 
					'(96 -- -- 99 -- 83 -- -- -- -- 56) 	
					'(95 -- -- -- -- -- -- -- -- -- 57 ) 
					'(92 -- -- -- -- -- -- -- -- -- 58) 
					'(91 90 89 78 77 76 75 74 73 60 59)))
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list 0 0 1 0 0 0 0 0 1 1 1 
			  			      1 0 1 1 1 1 1 1 1 0 0
			  			      1 1 1 1 1 1 1 1 1 1 1 
			 			      1 0 1 1 1 1 1 0 0 0 0
			 			      1 1 0 0 1 1 0 0 1 1 0 
			 			      0 0 0 0 0 1 1 1 1 1 1
		         			      1 1 1 1 1 1 0 0 0 0 0
			 			      0 1 1 1 1 0 1 1 1 1 1 
			  			      0 0 0 0 1 1 0 0 1 1 0
			  			      1 1 1 1 0 1 1 1 1 1 1
			 			      0 0 1 1 1 1 1 1 0 0 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )

	((equal i 14) (setq brd1 (list '(17 -- 19 -- 21 -- 45 -- 55 -- 97 -- 161) 
					'(-- 5 -- 7 -- 43 -- 53 -- 95 -- 163 --) 
					'(15 -- -- -- -- -- -- -- -- -- -- -- 159) 
					'(-- 3 -- -- -- -- -- -- -- -- -- 165 --) 
					'(13 -- -- -- -- -- -- -- -- -- -- -- 157) 
					'(-- 29 -- -- -- -- -- 61 -- -- -- 167 --) 
					'(31 -- -- -- -- -- -- -- -- -- -- -- 155) 
					'(-- 69 -- -- -- -- -- -- -- -- -- 169 --) 
					'(71 -- -- -- -- -- -- -- -- -- -- -- 153)
				        '(-- 73 -- -- -- -- -- 85 -- -- -- 149 --) 
					'(117 -- -- -- -- -- -- -- -- -- -- -- 147) 
					'(-- 121 -- 125 -- 129 -- 133 -- 137 -- 145 --) 
					'(119 -- 123 -- 127 -- 131 -- 135 -- 141 -- 143)))
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list   1 1 0 1 0 1 0 1 1 1
							1 1 0 1 0 1 0 1 0 1
							0 1 1 1 1 1 1 1 0 1
							0 1 1 1 1 1 1 1 1 1
							1 1 0 1 0 1 1 1 1 1
							1 1 0 1 0 1 1 1 1 1
							0 1 1 1 1 1 1 1 0 1
							0 1 0 1 1 1 1 1 1 1
							1 1 1 1 0 1 1 1 1 1
							1 1 1 1 0 1 0 1 1 1
							1 1 1 1 1 1 1 1 1 1
							1 1 1 1 1 1 0 1 0 1
							0 1 0 1 0 1 0 1 0 1
							0 1 0 1 0 1 0 1 1 1
							0 1 0 1 0 1 0 1 0 1
							1 1 0 1 0 1 0 1 0 1
							0 1 0 1 0 1 0 1 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri))		

	((equal i 15) (setq brd1 (list '(41 -- -- -- -- -- -- -- -- -- -- -- -- -- 211) 
					'(-- 43 -- -- -- -- -- -- -- -- -- -- -- 77 --) 
					'(-- -- 197 -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- 89 -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- 159 -- -- -- -- -- 143 -- -- -- --) 	
					'(-- -- -- -- -- 119 -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- 137 -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- 135 -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- 123 -- 125 -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- 149 -- -- -- -- --) 
					'(-- -- -- -- 97 -- -- -- -- -- 103 -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- 179 -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- 65 -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- 1 --) 
					'(27 -- -- -- -- -- -- -- -- -- -- -- -- -- 225)))
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list   0 1 1 1 1 1 1 1 1 1 1 1 1 1 1
			  				1 1 1 1 1 1 1 1 1 1 1 0 1 1 1
			  				1 1 1 1 1 1 1 1 1 1 0 1 0 1 1
			  				1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
			 			        1 1 1 1 0 1 1 1 1 1 1 1 1 1 1
			  				1 0 1 1 1 1 1 1 1 1 1 1 1 0 1
			 			        1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 
			  				1 1 1 1 1 1 1 1 1 1 1 1 1 0 1
			  				1 1 0 1 0 1 1 1 1 1 1 1 1 1 0 
			  				1 0 1 1 1 1 1 0 1 1 1 1 1 0 1
			  				1 1 1 1 1 1 1 1 0 1 1 1 1 1 1
			  				1 1 1 1 1 1 1 1 1 1 1 1 1 0 1
		    	  				1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
			  				1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
			  				0 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) )	

	((equal i 16) (setq brd1 (list '(43 -- 41 -- 39 -- 33 -- 27) 
					'(-- -- -- -- -- -- -- -- --) 
					'(51 -- -- -- -- -- -- -- 25) 
					'(-- -- -- -- -- -- -- -- --) 
					'(53 -- -- -- -- -- -- -- 23) 
					'(-- -- -- -- -- -- -- -- --) 
					'(55 -- -- -- -- -- -- -- 21) 
					'(-- -- -- -- -- -- -- -- --) 
					'(57 -- 81 -- 71 -- 7 -- 9)))
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
 				(board new_list) (printing n1)
				(setq available (list   0 1 1 1 1 1 0 1 0 1
							1 1 1 1 1 1 1 1 1 1
							0 1 0 1 0 1 0 1 1 1
							1 1 0 1 1 1 1 1 0 1
							0 1 0 1 1 1 1 1 1 1
							0 1 0 1 0 1 0 1 1 1
							1 1 1 1 1 1 1 1 1 1
							0 1 1 1 1 1 1 1 1 1 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (set_value1 6 6 1 new_list) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1)) 
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri))				

	((equal i 17) (setq brd1 (list '(1 -- 3 -- 19 -- 23 -- 25) 
					'(-- -- -- -- -- -- -- -- --) 
					'(7 -- -- -- -- -- -- -- 27) 
					'(-- -- -- -- -- -- -- -- --) 
					'(75 -- -- -- -- -- -- -- 35) 
					'(-- -- -- -- -- -- -- -- --) 
					'(81 -- -- -- -- -- -- -- 45) 
					'(-- -- -- -- -- -- -- -- --) 
					'(61 -- 59 -- 57 -- 49 -- 47)))
				(setq n1 (list-length brd1))
				(setq new_list (make_list brd1))
				(board new_list) (printing n1)
				(setq available (list   0 1 0 1 1 1 0 1 1 1
							1 1 1 1 1 1 1 1 0 1
							1 1 0 1 0 1 0 1 1 1
							1 1 1 1 0 1 1 1 1 1
							1 1 1 1 0 1 0 1 0 1
							1 1 1 1 1 1 0 1 0 1
							0 1 1 1 1 1 1 1 1 1
							1 1 1 1 0 1 1 1 1 1 0))
				(setq avail (make_list1 available))
				(cond ((equal type 1) (start_play new_list brd1))
					((equal type 2) (setq start_time (get-internal-run-time)) (auto new_list avail) (setq end_time (get-internal-run-time)) (terpri)
	(princ "AI RESULT: ") (terpri) (board new_list) (printing n1))
						(t nil) ) (terpri) (princ "*****END GAME*****") (terpri) ) )
	(terpri) (princ "CHECKING THE PATTERN") (terpri)
	(check_board new_list)
	
	(if (equal type 2) (progn
				(terpri) (princ "TIME TAKEN BY AUTO-PLAY:  ") (terpri)
				(princ (float (/ (- end_time start_time) internal-time-units-per-second)))
				(princ "seconds") (terpri)
			   )
	nil)
	(terpri) (princ "DO YOU WANT TO PLAY AGAIN, PRESS 1 IF YES OTHERWISE PRESS ANYTHING (EXCEPT 1): ")
	(setq answer (read))
	(cond ((equal answer 1) (numbrix))
		(t nil) )
	
)

(defun make_list(brd)
	(cond ((null brd) nil)
		(t (cons (make_sublist (car brd)) (make_list (cdr brd))) ) )
)

(defun make_sublist(row)
	(cond ((null row) nil)
		(t (cons (car row) (make_sublist (cdr row))) ) )
)

(defun make_list1(av)
	(cond ((null av) nil)
		(t (cons (car av) (make_list1 (cdr av))) ) )
)

(defun get_value(row col brd)
	(nth (1- col)
		(nth (- (list-length brd) row) brd) ) 
 )


(defun set_value(row col v brd)
	(setf (nth (1- col)
		(nth (- (list-length brd) row) brd) )
			v ) 
	(board brd)
)

(defun set_value1(row col v brd)
	(setf (nth (1- col)
		(nth (- (list-length brd) row) brd) )
			v ) 
)


(defun board(brd)
	(setq n (list-length brd))
	(setq axis n)
	(p_board brd n axis) 
)


(defun p_board(pbrd n axis_y)
	(cond ((null pbrd) (terpri)
		(cond ((<= n 3) (princ "     ") (board_double n) )
			(t (princ "     ") (board_triple n) ) ) )
	(t (terpri)
		(cond ((<= n 3) (princ "     ") (board_double n) )
			(t (princ "     ") (board_triple n)) )
				(terpri)
				(cond ((equal (check_digits 0 axis_y) 1) (princ "  ") (princ axis_y) (princ "  "))
					((equal (check_digits 0 axis_y) 2) (princ " ") (princ axis_y) (princ "  ")))	
					(p_row (car pbrd) n)
					(p_board (cdr pbrd) n (1- axis_y))) )
) 
				
(defun check_digits(counter num)
	(if (= num 0) counter
	(progn
		(setq num (floor (/ num 10)))
        	(setq counter (1+ counter))
		(check_digits counter num)))
)

(defun printing(n)
	(if (<= n 3) (progn
			(terpri) (princ "      ") (print_axis1 1 n))
		     (progn
			(terpri) (princ "       ") (print_axis2 1 n)))
)

(defun print_axis1(xval axis_x)
	(cond ((> xval axis_x) (terpri) (terpri)) 
	 	;;((equal xval 1) (princ xval) (princ "   ") (print_axis1 (1+ xval) axis_x))
		(t  (princ xval) (princ "  ") (print_axis1 (1+ xval) axis_x))
	)
)

(defun print_axis2(xval axis_x)
	(cond ((> xval axis_x) (terpri) (terpri)) 
		(t  (cond ((equal (check_digits 0 xval) 1) (princ xval) (princ "   ") (print_axis2 (1+ xval) axis_x))
			   ((equal (check_digits 0 xval) 2) (princ xval) (princ "  ") (print_axis2 (1+ xval) axis_x))
		     )
		)
	)
)

(defun board_double(n)
	(cond ((equal n 0) (princ "+"))
		(t (princ "+--")
		 (board_double (1- n)) ) ) )

(defun board_triple(n)
	(cond ((equal n 0) (princ "+"))
		(t (princ "+---")
			(setq n (1- n)) (board_triple n) ) ) )


(defun p_row(row n)
	(cond ((<= n 3) (p_row1 row))
	(t (p_row2 row n)))
)

(defun p_row1(row)
	(cond ((null row)(princ "| "))
		(t (princ "| ")
			(princ (car row))
				(p_row1 (cdr row)) ) ) 
)


(defun p_row2(row n)
	(if (<= n 9)
		(cond ((null row)(princ "|"))
			(t (princ "| ")
			(setq numb (car row))
			(setf digit numb)
			(princ numb)
			(setq count 0) 
			(cond ((equal digit '--) nil)
              		(t (loop 
                    		(setq digit (floor (float (/ digit 10))))
                    		(setq count (1+ count))
                    		(when (equal digit 0) (return t))
			    )
			    (cond ((equal count 1) (princ " ") ) ) ) )
			(p_row2 (cdr row) n) ) ) 
	(cond ((null row)(princ "| "))
			(t (princ "|")
			(setq numb (car row))
			(setf digit numb)
			(princ numb)
			(setq count 0) 
			(cond ((equal digit '--) (princ " "))
              		(t (loop 
                    		(setq digit (floor (float (/ digit 10))))
                    		(setq count (1+ count))
                    		(when (equal digit 0) (return t))
			    )
			(cond ((equal count 1) (princ "  ") ) ((equal count 2) (princ " ") ) ) ) )
		    (p_row2 (cdr row) n) ) ) 
	)
)

(defun check_board(brd)
	(setq row1 1)
	(setq col1 1)
	(setq n1 (list-length brd))
	(cond ((equal n1 5) (setq b1 (list '(-- -- -- -- --) 
					    '(-- -- -- -- --) 
					    '(-- -- -- -- --) 
					    '(-- -- -- -- --) 
					    '(-- -- -- -- --) ) )
							 (setq list_empty (make_list b1))
							(look_start row1 col1 brd list_empty n1) )

		((equal n1 6) (setq b1 (list '(-- -- -- -- -- --) 
					   '(-- -- -- -- -- --) 
					   '(-- -- -- -- -- --) 
					   '(-- -- -- -- -- --)
					   '(-- -- -- -- -- --) 
  					   '(-- -- -- -- -- --) ) )
							 (setq list_empty (make_list b1))
							(look_start row1 col1 brd list_empty n1) )

		((equal n1 7) (setq b1 (list '(-- -- -- -- -- -- --) 
					   '(-- -- -- -- -- -- --) 
					   '(-- -- -- -- -- -- --) 
					   '(-- -- -- -- -- -- --) 
					   '(-- -- -- -- -- -- --) 
			  		   '(-- -- -- -- -- -- --) 
					   '(-- -- -- -- -- -- --) ) )
							 (setq list_empty (make_list b1))
							(look_start row1 col1 brd list_empty n1) )
		

		((equal n1 8) 	(setq b1 (list '(-- -- -- -- -- -- -- --) 
		 				'(-- -- -- -- -- -- -- --)
					        '(-- -- -- -- -- -- -- --)
		 				'(-- -- -- -- -- -- -- --)
		 				'(-- -- -- -- -- -- -- --)
		 				'(-- -- -- -- -- -- -- --)
		 				'(-- -- -- -- -- -- -- --)
		 				'(-- -- -- -- -- -- -- --) ) ) 
						(setq list_empty (make_list b1))
					 (look_start row1 col1 brd list_empty n1) )

		((equal n1 9) (setq b1 (list '(-- -- -- -- -- -- -- -- --) 
				      	     '(-- -- -- -- -- -- -- -- --) 
				      	     '(-- -- -- -- -- -- -- -- --) 
				      	     '(-- -- -- -- -- -- -- -- --) 
				      	     '(-- -- -- -- -- -- -- -- --) 
				      	     '(-- -- -- -- -- -- -- -- --) 
				      	     '(-- -- -- -- -- -- -- -- --) 
				      	     '(-- -- -- -- -- -- -- -- --) 
				      	     '(-- -- -- -- -- -- -- -- --)))
							 (setq list_empty (make_list b1))
							(look_start row1 col1 brd list_empty n1) )

		((equal n1 10) (setq b1 (list '(-- -- -- -- -- -- -- -- -- --) 
				 	'(-- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- --) 		
					'(-- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- --)
					'(-- -- -- -- -- -- -- -- -- --)
				        '(-- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- --)))
							 (setq list_empty (make_list b1))
							(look_start row1 col1 brd list_empty n1) )

		((equal n1 11) (setq b1 (list '(-- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- --) 	
					'(-- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- --)))
							 (setq list_empty (make_list b1))
							(look_start row1 col1 brd list_empty n1) )
		((equal n1 13) (setq b1 (list '(-- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- --)
				        '(-- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- --)))
							 (setq list_empty (make_list b1))
							(look_start row1 col1 brd list_empty n1) )

		((equal n1 15) (setq b1 (list '(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 	
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --) 
					'(-- -- -- -- -- -- -- -- -- -- -- -- -- -- --)))
							 (setq list_empty (make_list b1))
							(look_start row1 col1 brd list_empty n1) )

			(t nil) ) 
)

(defun look_start(row2 col2 brd empty_b1 n1)
	(cond ((and (>= row2 n1) (> col2 n1)) (terpri) (princ "LOST!!") nil)
	(t (cond ( (and (< row2 n1) (> col2 n1)) (setq row2 (1+ row2)) (setq col2 1) )
		(t nil))
	(setq val2 (get_value row2 col2 brd))
	(cond ( (equal val2 1) (set_value1 row2 col2 val2 empty_b1) (board empty_b1) (printing n1) (checking val2 row2 col2 brd empty_b1))
		(t (setq col2 (1+ col2)) (look_start row2 col2 brd empty_b1 n1) ) ) ) )
)

(defun checking(val3 row3 col3 brd br)
	(setq n1 (list-length brd))
	(setq num (1+ val3))
	(setq y1 (1+ row3))
	(setq y2 (1- row3))
	(setq y3 (1+ col3))
	(setq y4 (1- col3))
	(setq max (* n1 n1))
	

	(setq x1 (if (<= y1 n1) (if
                                     (equal num (get_value y1 col3 brd)) 
					(progn (set_value1 y1 col3 num br) (board br))
                                   nil)
                     nil))

	(setq x2 (if (> y2 0) (if
                                     (equal num (get_value y2 col3 brd)) 
					(progn (set_value1 y2 col3 num br) (board br))
                                   nil)
                     nil))
		
	
	(setq x3 (if (<= y3 n1) (if
                                     (equal num (get_value row3 y3 brd)) 
					(progn (set_value1 row3 y3 num br) (board br))
                                   nil)
                     nil))
	

	(setq x4 (if (> y4 0) (if
                                     (equal num (get_value row3 y4 brd)) 
					(progn (set_value1 row3 y4 num br) (board br))
                                   nil)
                     nil))

	(cond ( (equal num max) (printing n1) (terpri) (princ "WON!!") )
	      ( (not (null x1)) (printing n1) (checking num y1 col3 brd br))
	      ( (not (null x2)) (printing n1) (checking num y2 col3 brd br))
	      ( (not (null x3)) (printing n1) (checking num row3 y3 brd br))
	      ( (not (null x4)) (printing n1) (checking num row3 y4 brd br))
	      (t (terpri) (princ "LOST!!"))
	)
 
)

(defun start_play(brd bd1)
	(terpri) (princ "ENTER THE NUMBER AND THE POSITION WHERE YOU WANT TO PLACE THE NUMBER") (terpri)
	(terpri) (princ "ROW:  ") 
	(princ "COLUMN:  ")
	(princ "NUMBER:  ")
	(terpri)
	(setq row (read))
	
	(setq col (read))
	
	(setq val (read)) 
	(setq n1 (list-length brd))

	(setq a (check_position_bound row col brd))
	
	(setq b (check_value_bound val brd))
	
	(setq c (cond ((and a b) (check_overwrite row col brd bd1) ) ) )
	

	(setq d (cond ((equal c t) (check_repeat val brd) ) ) )
	 

	 (cond ((and (and a b) (and c d) ) (set_value1 row col val brd) (board brd) (printing n1) (keep_play brd brd1) )
			(t  (start_play brd brd1)) ) 

)

(defun keep_play(brd brd1)
	(setq x (play brd))
	(cond ((null x) nil)
		(t (start_play brd brd1) ) )

)

(defun play(b)
	(cond   ((null b) nil)
		( (or (member '- (car b)) (member '-- (car b)))  t )
		(t (play (cdr b)) ) )	
)


(defun check_position_bound(row col brd)
	(setq n (list-length brd))
	(setq n (1- n) r (1- row) c (1- col) )
	(cond ( (and (and (<= r n) (>= r 0)) (and (<= c n) (>= c 0)) ) t )
	      ( t ( cond ( (and (or (> r n) (< r 0)) (or (> c n) (< c 0))) (terpri) (princ "ERROR: ROW AND COLUMN VALUE OUT OF BOUND") (terpri) nil )
		         ( (or (> r n) (< r 0)) (terpri) (princ "ERROR:ROW VALUE OUT OF BOUND") (terpri) nil )
		         ( (or (> c n) (< c 0)) (terpri) (princ "ERROR:COLUMN VALUE OUT OF BOUND") (terpri) nil )
		         ( t ) )
              )
	)
)


(defun check_repeat(v b)
	(cond ((null b) t)
	      ((member v (car b)) (terpri) (princ "ERROR:ENTERED VALUE ") (princ v) (princ " IS ALREADY PRESENT ON THE BOARD") (terpri) nil )
	      (t (check_repeat v (cdr b)) ) 
	)	
)

(defun check_repeat1(v b)
	(cond ((null b) t)
	      ((member v (car b)) nil )
	      (t (check_repeat v (cdr b)) ) 
	)	
)

(defun check_value_bound(v brd)
	(setq n (list-length brd))
	(cond ((> v (* n n)) (terpri) (princ "ERROR:VALUE CANNOT BE GREATER THAN ") (princ (* n n)) (terpri) nil )
	      ((< v 1) (terpri) (princ "ERROR:VALUE CANNOT BE LESS THAN 1") (terpri) nil )
	      (t)
	)	
)

(defun check_overwrite(row col brd bd1)
	(setq v (get_value row col brd))
	(setq check (check_repeat1 v bd1) )
	(cond ((or (equal v '-) (equal v '--)) t )
	      ((null check)(terpri)(princ "ERROR:CANNOT OVERWRITE A VALUE ON POSITION (" ) (princ row) (princ ",") (princ col) (princ ")") (terpri) nil)
	      (t (terpri)(princ "ARE YOU SURE YOU WANT TO OVERWRITE A VALUE, PRESS 1 IF YES OTHERWISE PRESS ANYTHING (EXCEPT 1): ") (setq answer (read)) (cond ((equal answer 1) t)
					(t nil)
				    )
	      )
	)
)

(defparameter fill_op1 0)
(defparameter fill_op2 0)
(defparameter fill_op3 0)
(defparameter fill_op4 0)


(defun sqr1 (col brd)
	(setq n (list-length brd))
	(nth (1- col) brd)
)

(defun set-sqr1 (col brd val)
	(setq n (list-length brd))
	(setf (nth (1- col) brd)
		val)
)

(defun auto(brd avl)
	(setq fill_op1 0)
	(setq fill_op2 0)
	(setq fill_op3 0)
	(setq fill_op4 0)
	(setq n (list-length brd))
	(fill_one_option brd 1 avl)	
	(fill_alternate brd 1 avl)
	(fill_two_option brd 1 avl)
	(cond ( (or (or (equal fill_op1 1) (equal fill_op2 1)) (equal fill_op3 1)) (auto brd avl))
		(t (fill_simple_distance brd 1 avl))
	)
	
	
	(loop for i from 1 to n do
        (loop for j from 1 to n do
              	(cond	((equal (dfs_tree i j 1 brd avl n) t) t)
                    	(t (continue))
		)
	))
)

(defun sqr(row col brd)
	(setq n (list-length brd))
	(nth (1- col)
		(nth (- n row) brd ))
)

(defun set-sqr(row col brd val)
	(setq n (list-length brd))
	(setf (nth (1- col)
		(nth (- n row) brd))
		val)
)

(defun dfs_tree(i j num brd avl n)
	(cond	((> num (* n n)) (return-from dfs_tree t))
		(t (continue))
	)
	(cond
		((equal (sqr1 num avl) 0) (cond 	((equal (sqr i j brd) num) (continue))
							(t (return-from dfs_tree nil))
						)
		)
		(t 	(cond 	((equal (sqr i j brd) '--) (continue))
				(t (return-from dfs_tree nil))
			)
			(set-sqr i j brd num)
			(set-sqr1 num avl 0)
			(set-sqr1 num flag 1)
		)
	)
	(cond 	((and (< j n) (dfs_tree i (1+ j) (1+ num) brd avl n)) (return-from dfs_tree t))
		(t (continue))
	)
	(cond 	((and (> j 1) (dfs_tree i (1- j) (1+ num) brd avl n)) (return-from dfs_tree t))
		(t (continue))
	)
	(cond 	((and (< i n) (dfs_tree (1+ i) j (1+ num) brd avl n)) (return-from dfs_tree t))
		(t (continue))
	)
	(cond 	((and (> i 1) (dfs_tree (1- i) j (1+ num) brd avl n)) (return-from dfs_tree t))
		(t (continue))
	)
	(cond	((equal (sqr1 num flag) 1) (set-sqr i j brd '--) (set-sqr1 num avl 1) (set-sqr1 num flag 0)))
	
	(cond 	((null (member 1 avl)) (return-from dfs_tree t))
		(t (return-from dfs_tree nil)))
)

(defun look_begin(row2 col2 brd n1 begin_val)
	(cond ( (and (>= row2 n1) (> col2 n1)) nil)
		(t (cond ( (and (< row2 n1) (> col2 n1)) (setq row2 (1+ row2)) (setq col2 1) )
			(t nil))	
			(setq val2 (get_value row2 col2 brd))
			(cond ( (equal val2 begin_val) (setq state (list val2 row2 col2)))
				(t (setq col2 (1+ col2)) (look_begin row2 col2 brd n1 begin_val) ) 
			)
		 )
	 )
)


(defun check_value(val brd)
	(cond ((null brd) nil)
		((member val (car brd)) t)
			(t (check_value val (cdr brd)) ) )
)


(defun fill_one_option(brd begin_val avl)
	(setq row2 1)
	(setq col2 1)
	(setq n (list-length brd))
	
	(cond ((<= begin_val (* n n)) (setq s (look_begin row2 col2 brd n begin_val))
						(cond ((null s) nil)
							(t (setq no (car s)) (setq ro (cadr s)) (setq co (caddr s))
								(check_neighbour1 no ro co brd n avl) ) )
				 		(fill_one_option brd (1+ begin_val) avl) )
		(t nil) )
)


(defun check_neighbour1(val row col brd n1 avl)

	(setq g (check_value (1+ val) brd) )
	(setq s (check_value (1- val) brd) )
		
	(cond ((or (and g s) (and (null g) (null s))) nil )
		 (t  (if (equal val 1)
				(cond ((null g) (filling1 (1+ val) row col brd n1 avl))
					(t nil)
		      		)
			(if (equal val (* n1 n1))
				(cond ((null s) (filling1 (1- val) row col brd n1 avl))
					(t nil)
		      		)
			(cond 
				((null s) (filling1 (1- val) row col brd n1 avl))
				((null g) (filling1 (1+ val) row col brd n1 avl))
				(t nil)
			) 
		      	)
		      )
		  )	
	)
)

(defun filling1(fill_val row col brd n1 avl)
	(setq option 0)

	(setq x1 (if (<= (1+ row) n1) (if
                                    (or (equal '- (get_value (1+ row) col brd)) (equal '-- (get_value (1+ row) col brd))) 
					(progn 
						(setq option (1+ option))
						t)
                                   nil)
                     nil))


	(setq x2 (if (> (1- row) 0) (if
                                     (or (equal '- (get_value (1- row) col brd)) (equal '-- (get_value (1- row) col brd))) 
					(progn 
						(setq option (1+ option))
						t)
                                   nil)
                     nil))
	
	(setq x3 (if (<= (1+ col) n1) (if
                                   (or (equal '- (get_value row (1+ col) brd)) (equal '-- (get_value row (1+ col) brd))) 
					(progn 
						(setq option (1+ option))
						t) 
                                   nil)
                     nil))

	(setq x4 (if (> (1- col) 0) (if
                                     (or (equal '- (get_value row (1- col) brd)) (equal '-- (get_value row (1- col) brd))) 
					(progn 
						(setq option (1+ option))
						t)
                                   nil)
                     nil))

	(cond ((equal option 1)
		(cond 	((equal x1 t) (set_value1 (1+ row) col fill_val brd)
				(set-sqr1 fill_val avl 0) 
				(if (equal fill_op1 0)
					(setq fill_op1 1)
				nil)
			)
			((equal x2 t) (set_value1 (1- row) col fill_val brd)
				(set-sqr1 fill_val avl 0)
				(if (equal fill_op1 0)
					(setq fill_op1 1)
				nil)
			)
			((equal x3 t) (set_value1 row (1+ col) fill_val brd) (set-sqr1 fill_val avl 0)
				(if (equal fill_op1 0)
					(setq fill_op1 1)
				nil)
			)
			((equal x4 t) (set_value1 row (1- col) fill_val brd) (set-sqr1 fill_val avl 0)
				(if (equal fill_op1 0)
					(setq fill_op1 1)
				nil)
			)
			(t nil)
		) )
	      (t nil)
	)
)

(defun fill_alternate(brd begin_val avl)
	(setq row2 1)
	(setq col2 1)
	(setq n (list-length brd))
	
	(cond ( (<= begin_val (- (* n n) 2)) (setq s (look_begin row2 col2 brd n begin_val))
						(cond ((null s) nil)
							(t (setq no (car s)) (setq ro (cadr s)) (setq co (caddr s))
								(check_alternate no ro co brd n avl) ) )
				 		(fill_alternate brd (1+ begin_val) avl) )
		(t nil) )
)


(defun check_alternate(val row col brd n1 avl)

	(setq g (check_value (1+ val) brd) )
	(setq gg (check_value (1+ (1+ val)) brd) )
		
	(cond ((equal g t) nil)
	      ((null gg) nil)
	      ((and (null g) (equal gg t)) 
			(filling2 (1+ val) row col brd n1 avl))
	      (t nil) 	
	)
)


(defun filling2(fill_val row col brd n1 avl)
	(setq option 0)
	(setq p1 0)
	(setq p2 0)
	
	(if (and (<= (1+ row) n1) (<= (1+ col) n1)) 
	 	(cond ( (equal (1+ fill_val) (get_value (1+ row) (1+ col) brd))
	      		(setq p1 (if (<= (1+ row) n1) (if
                                    (or (equal '- (get_value (1+ row) col brd)) (equal '-- (get_value (1+ row) col brd))) 
					(progn 
						(setq option (1+ option))
						t)
                                   nil)
                     		nil))
	
			(setq p2 (if (<= (1+ col) n1) (if
                                   (or (equal '- (get_value row (1+ col) brd)) (equal '-- (get_value row (1+ col) brd))) 
					(progn 
						(setq option (1+ option))
						t) 
                                   nil)
                    		 nil))

			(cond ((equal option 1)
				(cond 	((equal p1 t) (set_value1 (1+ row) col fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
					)
					((equal p2 t) (set_value1 row (1+ col) fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
					)
					(t nil) 
				) )
	      		(t nil) ) 
			) 
		(t nil) )		
	nil)

	(if (and (> (1- row) 0) (> (1- col) 0)) 
		(cond ((equal (1+ fill_val) (get_value (1- row) (1- col) brd))
	      		(setq p1 (if (> (1- row) 0) (if
                                    (or (equal '- (get_value (1- row) col brd)) (equal '-- (get_value (1- row) col brd))) 
					(progn 
						(setq option (1+ option))
						t)
                                   nil)	
				nil))	

			(setq p2 (if (> (1- col) 0) (if
                                   (or (equal '- (get_value row (1- col) brd)) (equal '-- (get_value row (1- col) brd))) 
					(progn 
						(setq option (1+ option))
						t) 
                                   nil)
                     		nil))
			(cond ((equal option 1)
				(cond 	((equal p1 t) (set_value1 (1- row) col fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
					)
					((equal p2 t) (set_value1 row (1- col) fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
					)
					(t nil) ))
			(t nil) ) ) 
		(t nil) )		
	nil)
	      	
	
	(if (and (> (1- row) 0) (<= (1+ col) n1)) 
		(cond ((equal (1+ fill_val) (get_value (1- row) (1+ col) brd))
	      	(setq p1 (if (> (1- row) 0) (if
                                    (or (equal '- (get_value (1- row) col brd)) (equal '-- (get_value (1- row) col brd))) 
					(progn 
						(setq option (1+ option))
						t)
                                   nil)
                     nil))	
		(setq p2 (if (<= (1+ col) n1) (if
                                   (or (equal '- (get_value row (1+ col) brd)) (equal '-- (get_value row (1+ col) brd))) 
					(progn 
						(setq option (1+ option))
						t) 
                                   nil)
                     nil))
		(cond ((equal option 1)
			(cond 	((equal p1 t) (set_value1 (1- row) col fill_val brd) (set-sqr1 fill_val avl 0)
					(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
				)
				((equal p2 t) (set_value1 row (1+ col) fill_val brd) (set-sqr1 fill_val avl 0)
					(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
				)
				(t nil) ) )
			(t nil) ) ) 
		(t nil) )		
	nil)
	   
	(if (and (<= (1+ row) n1) (> (1- col) 0)) 
		(cond ((equal (1+ fill_val) (get_value (1+ row) (1- col) brd))
	      	(setq p1 (if (<= (1+ row) n1) (if
                                    (or (equal '- (get_value (1+ row) col brd)) (equal '-- (get_value (1+ row) col brd))) 
					(progn 
						(setq option (1+ option))
						t)
                                   nil)
                     nil))	
		(setq p2 (if (> (1- col) 0) (if
                                   (or (equal '- (get_value row (1- col) brd)) (equal '-- (get_value row (1- col) brd))) 
					(progn 
						(setq option (1+ option))
						t) 
                                   nil)
                     nil))
		(cond ((equal option 1)
			(cond 	((equal p1 t) (set_value1 (1+ row) col fill_val brd) (set-sqr1 fill_val avl 0)
					(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
				)
				((equal p2 t) (set_value1 row (1- col) fill_val brd) (set-sqr1 fill_val avl 0)
					(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
				)
				(t nil) ) )
	      		(t nil) ) )
	 	(t nil) )		
	nil)

	(if (<= (+ row 2) n1) 
		(cond ((equal (1+ fill_val) (get_value (+ row 2) col brd)) (set_value1 (1+ row) col fill_val brd) (set-sqr1 fill_val avl 0)
				(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
			 )
			(t nil) )
	nil)
	
	(if (> (- row 2) 0) 
		(cond ((equal (1+ fill_val) (get_value (- row 2) col brd)) (set_value1 (1- row) col fill_val brd) (set-sqr1 fill_val avl 0)
				(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
			)
			(t nil) )
	nil)

	(if (<= (+ col 2) n1) 
		(cond ((equal (1+ fill_val) (get_value row (+ col 2) brd)) (set_value1 row (1+ col) fill_val brd) (set-sqr1 fill_val avl 0)
				(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
			 )
			(t nil) )
	nil)
	
	(if (> (- col 2) 0) 
		(cond ((equal (1+ fill_val) (get_value row (- col 2) brd)) (set_value1 row (1- col) fill_val brd) (set-sqr1 fill_val avl 0)
				(if (equal fill_op2 0)
							(setq fill_op2 1)
						nil)
			)
			(t nil) )
	nil)
)

(defun fill_two_option(brd begin_val avl)
	(setq row2 1)
	(setq col2 1)
	(setq n (list-length brd))
	
	(cond ((< begin_val (* n n)) (setq s (look_begin row2 col2 brd n begin_val))
						(cond ((null s) nil)
							(t (setq no (car s)) (setq ro (cadr s)) (setq co (caddr s))
								(check_neighbour2 no ro co brd n avl) ) )
				 		(fill_two_option brd (1+ begin_val) avl) )
		(t nil) )
)

(defun check_neighbour2(val row col brd n1 avl)

	(setq g (check_value (1+ val) brd) )
	(setq s (check_value (1- val) brd) )
		
	(cond ((or (and g s) (and (null g) (null s))) nil )
		 (t  (if (equal val 1)
				(cond ((null g) (filling3 (1+ val) row col brd n1 avl))
					(t nil)
		      		)
			(cond 
				((null s) (filling3 (1- val) row col brd n1 avl))
				((null g) (filling3 (1+ val) row col brd n1 avl))
				(t nil)
		      	)
		      )
		  )	
	)
)

(defun filling3(fill_val row col brd n1 avl)
	(setq option 0)

	(setq x1 (if (<= (1+ row) n1) (if
                                    (or (equal '- (get_value (1+ row) col brd)) (equal '-- (get_value (1+ row) col brd))) 
					(progn 
						(setq option (1+ option))
						t)
                                   nil)
                     nil))


	(setq x2 (if (> (1- row) 0) (if
                                     (or (equal '- (get_value (1- row) col brd)) (equal '-- (get_value (1- row) col brd))) 
					(progn 
						(setq option (1+ option))
						t)
                                   nil)
                     nil))
	
	(setq x3 (if (<= (1+ col) n1) (if
                                   (or (equal '- (get_value row (1+ col) brd)) (equal '-- (get_value row (1+ col) brd))) 
					(progn 
						(setq option (1+ option))
						t) 
                                   nil)
                     nil))

	(setq x4 (if (> (1- col) 0) (if
                                     (or (equal '- (get_value row (1- col) brd)) (equal '-- (get_value row (1- col) brd))) 
					(progn 
						(setq option (1+ option))
						t)
                                   nil)
                     nil))

	(cond ((equal option 2)
		(cond 	((and x1 x3) 
				 (setq pos1 (check_up (1+ row) col brd n1))
			         (setq pos2 (check_right row (1+ col) brd n1))
				 (cond ((or (and pos1 pos2) (and (null pos1) (null pos2))) nil)
				       ((null pos1) (set_value1 row (1+ col) fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op3 0)
							(setq fill_op3 1)
						nil)
					)
				       ((null pos2) (set_value1 (1+ row) col fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op3 0)
							(setq fill_op3 1)
						nil)
					)
				       (t nil) 
				 )		
			 )
			 ((and x2 x3) 
				 (setq pos1 (check_down (1- row) col brd n1))
			         (setq pos2 (check_right row (1+ col) brd n1))
				 (cond ((or (and pos1 pos2) (and (null pos1) (null pos2))) nil)
				       ((null pos1) (set_value1 row (1+ col) fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op3 0)
							(setq fill_op3 1)
						nil)
					)
				       ((null pos2) (set_value1 (1- row) col fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op3 0)
							(setq fill_op3 1)
						nil)
					)
				       (t nil) 
				 )		
			 )
			 ((and x2 x4) 
				 (setq pos1 (check_down (1- row) col brd n1))
			         (setq pos2 (check_left row (1- col) brd n1))
				 (cond ((or (and pos1 pos2) (and (null pos1) (null pos2))) nil)
				       ((null pos1) (set_value1 row (1- col) fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op3 0)
							(setq fill_op3 1)
						nil)
					)
				       ((null pos2) (set_value1 (1- row) col fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op3 0)
							(setq fill_op3 1)
						nil)
					)
				       (t nil) 
				 )		
			 )
			 ((and x1 x4) 
				 (setq pos1 (check_up (1+ row) col brd n1))
			         (setq pos2 (check_left row (1- col) brd n1))
				 (cond ((or (and pos1 pos2) (and (null pos1) (null pos2))) nil)
				       ((null pos1) (set_value1 row (1- col) fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op3 0)
							(setq fill_op3 1)
						nil)
					)
				       ((null pos2) (set_value1 (1+ row) col fill_val brd) (set-sqr1 fill_val avl 0)
						(if (equal fill_op3 0)
							(setq fill_op3 1)
						nil)
					)
				       (t nil) 
				 )		
			 )
			(t nil)
		) )
	      (t nil)
	)
)

(defun check_up(row col brd n1)
	
	(setq blank 0) 
	(setq values 0)

	 (if (<= (1+ row) n1)  (progn 
				(setq val_up (get_value (1+ row) col brd))
				(if (or (equal '- val_up) (equal '-- val_up)) 
					(setq blank (1+ blank))	
                             	    (cond ((equal val_up 1)
						(if (check_value (1+ val_up) brd)
						(setq values (1+ values)) nil))
					  ( t (if (and (check_value (1+ val_up) brd) (check_value (1- val_up) brd))
						(setq values (1+ values))
				    		nil) ) 
				     ) 
			      )
			)	
        	nil)
		(if (<= (1+ col) n1)  (progn 
				(setq val_up (get_value row (1+ col) brd))
				(if (or (equal '- val_up) (equal '-- val_up)) 
					(setq blank (1+ blank))
                             	    (cond ((equal val_up 1)
						(if (check_value (1+ val_up) brd)
						(setq values (1+ values)) nil))
					  ( t (if (and (check_value (1+ val_up) brd) (check_value (1- val_up) brd))
						(setq values (1+ values))
				    		nil) ) 
				      ) 
			      )	
			)
        	nil)
		(if (> (1- col) 0)  (progn 
				(setq val_up (get_value row (1- col) brd))
				(if (or (equal '- val_up) (equal '-- val_up)) 
					(setq blank (1+ blank))	
                             	    (cond ((equal val_up 1)
						(if (check_value (1+ val_up) brd)
						(setq values (1+ values)) nil))
					  ( t (if (and (check_value (1+ val_up) brd) (check_value (1- val_up) brd))
						(setq values (1+ values))
				    		nil) ) 
				     ) 
			      )	
			)
        	nil)
		(cond ((and (equal blank 1) (equal values 2)) t)
			(t nil) )
)

(defun check_down(row col brd n1)
	
	(setq blank 0) 
	(setq values 0)

	(if (> (1- row) 0)  (progn 
				(setq val_down (get_value (1- row) col brd))
				(if (or (equal '- val_down) (equal '-- val_down)) 
					(setq blank (1+ blank))	
                             	    (cond ((equal val_down 1)
                                          (if (check_value (1+ val_down) brd)
						          (setq values (1+ values)) 
                                              nil))
					      (t (if (and (check_value (1+ val_down) brd) (check_value (1- val_down) brd))
                                              (setq values (1+ values))
				    		    nil) ) 
                                   ) 
			      )	
			)
        	nil)
		(if (<= (1+ col) n1)  (progn 
				(setq val_down (get_value row (1+ col) brd))
				(if (or (equal '- val_down) (equal '-- val_down)) 
					(setq blank (1+ blank))
                             	    (cond ((equal val_down 1)
						(if (check_value (1+ val_down) brd)
						(setq values (1+ values)) nil))
					  ( t (if (and (check_value (1+ val_down) brd) (check_value (1- val_down) brd))
						(setq values (1+ values))
				    		nil) )  
                                ) 
			      )	
			)
        	nil)
		(if (> (1- col) 0)  (progn 
				(setq val_down (get_value row (1- col) brd))
				(if (or (equal '- val_down) (equal '-- val_down)) 
					(setq blank (1+ blank))	
                             	    (cond ((equal val_down 1)
						(if (check_value (1+ val_down) brd)
						(setq values (1+ values)) nil))
					  ( t (if (and (check_value (1+ val_down) brd) (check_value (1- val_down) brd))
						(setq values (1+ values))
				    		nil) ) 
				      ) 
			      )	
			)
        	nil)
		(cond ((and (equal blank 1) (equal values 2)) t)
			(t nil) )		
)

(defun check_right(row col brd n1)
	
	(setq blank 0) 
	(setq values 0)

	(if (<= (1+ col) n1)  (progn 
				(setq val_up (get_value row (1+ col) brd))
				(if (or (equal '- val_up) (equal '-- val_up)) 
					(setq blank (1+ blank))	
                             	    (cond ((equal val_up 1)
						(if (check_value (1+ val_up) brd)
						(setq values (1+ values)) nil))
					  ( t (if (and (check_value (1+ val_up) brd) (check_value (1- val_up) brd))
						(setq values (1+ values))
				    		nil) ) 
				     ) 
			      )	
			)
        	nil)
		(if (<= (1+ row) n1)  (progn 
				(setq val_up (get_value (1+ row) col brd))
				(if (or (equal '- val_up) (equal '-- val_up)) 
					(setq blank (1+ blank))
                             	    (cond ((equal val_up 1)
						(if (check_value (1+ val_up) brd)
						(setq values (1+ values)) nil))
					  ( t (if (and (check_value (1+ val_up) brd) (check_value (1- val_up) brd))
						(setq values (1+ values))
				    		nil) ) 
				     ) 
			      )
			)	
        	nil)
		(if (> (1- row) 0)  (progn 
				(setq val_up (get_value (1- row) col brd))
				(if (or (equal '- val_up) (equal '-- val_up)) 
					(setq blank (1+ blank))	
                             	    (cond ((equal val_up 1)
						(if (check_value (1+ val_up) brd)
						(setq values (1+ values)) nil))
					  ( t (if (and (check_value (1+ val_up) brd) (check_value (1- val_up) brd))
						(setq values (1+ values))
				    		nil) ) 
				    ) 
			      )
			)	
        	nil)
		(cond ((and (equal blank 1) (equal values 2)) t)
			(t nil) )
)


(defun check_left(row col brd n1)
	
	(setq blank 0) 
	(setq values 0)

	 (if (> (1- col) 0)  (progn 
				(setq val_up (get_value row (1- col) brd))
				(if (or (equal '- val_up) (equal '-- val_up)) 
					(setq blank (1+ blank))	
                             	    (cond ((equal val_up 1)
						(if (check_value (1+ val_up) brd)
						(setq values (1+ values)) nil))
					  ( t (if (and (check_value (1+ val_up) brd) (check_value (1- val_up) brd))
						(setq values (1+ values))
				    		nil) ) 
			 	     ) 
			      )	
			)
        	nil)
		(if (<= (1+ row) n1)  (progn 
				(setq val_up (get_value (1+ row) col brd))
				(if (or (equal '- val_up) (equal '-- val_up)) 
					(setq blank (1+ blank))
                             	    (cond ((equal val_up 1)
						(if (check_value (1+ val_up) brd)
						(setq values (1+ values)) nil))
					  ( t (if (and (check_value (1+ val_up) brd) (check_value (1- val_up) brd))
						(setq values (1+ values))
				    		nil) ) 
				     ) 
			      )
			)	
        	nil)
		(if (> (1- row) 0)  (progn 
				(setq val_up (get_value (1- row) col brd))
				(if (or (equal '- val_up) (equal '-- val_up)) 
					(setq blank (1+ blank))	
                             	    (cond ((equal val_up 1)
						(if (check_value (1+ val_up) brd)
						(setq values (1+ values)) nil))
					  ( t (if (and (check_value (1+ val_up) brd) (check_value (1- val_up) brd))
						(setq values (1+ values))
				    		nil) ) 
				    ) 
			      )	
			)
        	nil)
		(cond ((and (equal blank 1) (equal values 2)) t)
			(t nil) )
)

(defun fill_simple_distance(brd begin_val avl)
	(setq row2 1)
	(setq col2 1)
	(setq n (list-length brd))
	
	(cond ((< begin_val (* n n)) (setq s (look_begin row2 col2 brd n begin_val))
						(cond ((null s) nil)
							(t (setq no (car s)) (setq ro (cadr s)) (setq co (caddr s))
								(check_greater no ro co brd n avl) ) )
				 		(fill_simple_distance brd (1+ begin_val) avl) )
		(t nil) )
)

(defun check_greater(val row col brd n1 avl)
	(setq first_num val)
	(setq g (check_value (1+ val) brd) )
	(cond ((null g) (next_number first_num val row col brd n1 0 avl))
		(t nil)
	)
		
)

(defun next_number(first_num val row col brd n1 dist avl)
	(cond ((equal val (1- (* n1 n1))) (setq dist (1+ dist))	
			(if (check_value (1+ val) brd)
				(progn (setq no_dist (list (1+ val) (1- dist)))
					(filling4 no_dist row col first_num brd n1 avl)
				)	
			 nil)	 
	      )
	      (t (setq g (check_value (1+ val) brd)) (setq dist (1+ dist))
			(if (null g)  
			        (next_number first_num (1+ val) row col brd n1 dist avl)
			     (progn (setq no_dist (list (1+ val) (1- dist)))
					(filling4 no_dist row col first_num brd n1 avl)
			     )
	         	) 			    			
	      )
	)
)

(defun filling4(no_dist row1 col1 val1 brd n1 avl)
	(cond ( (null (car no_dist)) (setq val2 (* n1 n1)) )
	      (t (setq val2 (car no_dist)) )
	)
	(setq dist (cadr no_dist))
	(setq nxt (look_begin 1 1 brd n1 val2))
	(setq row2 (cadr nxt))
	(setq col2 (caddr nxt))	
	(setq same_up 0)
	(setq same_down 0) 
	(setq same_left 0)
	(setq same_right 0)
	(setq fill_val (1+ val1))
	(setq col_sub (1- col1))
	(setq col_add (1+ col1))
	(setq row_sub (1- row1))
	(setq row_add (1+ row1))
	
	
	(cond   ((and (and (equal row2 row1) (< col2 col1)) (equal (1- (- col1 col2)) dist)) (setq same_left 1)
			(loop
				(set_value1 row1 col_sub fill_val brd) (set-sqr1 fill_val avl 0)
				(setq fill_val (1+ fill_val))
				(setq col_sub (1- col_sub))
			(when (equal col2 col_sub) (return nil))
			)
			(if (equal fill_op4 0)
					(setq fill_op4 1)
				nil)				
	        )
		((and (and (equal row2 row1) (> col2 col1)) (equal (1- (- col2 col1)) dist)) (setq same_right 1)
			(loop
				(set_value1 row1 col_add fill_val brd) (set-sqr1 fill_val avl 0)
				(setq fill_val (1+ fill_val))
				(setq col_add (1+ col_add))
			(when (equal col2 col_add) (return nil))
			)
			(if (equal fill_op4 0)
					(setq fill_op4 1)
				nil)
		)
		((and (and (equal col2 col1) (> row2 row1)) (equal (1- (- row2 row1)) dist)) (setq same_up 1)
			(loop
				(set_value1 row_add col1 fill_val brd) (set-sqr1 fill_val avl 0)
				(setq fill_val (1+ fill_val))
				(setq row_add (1+ row_add))
			(when (equal row2 row_add) (return nil))
			)
			(if (equal fill_op4 0)
					(setq fill_op4 1)
				nil)
		)
		((and (and (equal col2 col1) (< row2 row1)) (equal (1- (- row1 row2)) dist)) (setq same_down 1)
			(loop
				(set_value1 row_sub col1 fill_val brd) (set-sqr1 fill_val avl 0)
				(setq fill_val (1+ fill_val))
				(setq row_sub (1- row_sub))
			(when (equal row2 row_sub) (return nil))
			)
			(if (equal fill_op4 0)
					(setq fill_op4 1)
				nil)
		)
		(t nil)		
	)

	(if (equal dist 2)
		(cond ((and (equal row2 row1) (or (equal col2 (1+ col1)) (equal col2 (1- col1))))
				(if (<= (1+ row1) n1) (setq dp1 (get_value (1+ row1) col1 brd)) nil)
				(if (<= (1+ row1) n1) (setq dp2 (get_value (1+ row1) col2 brd)) nil)
				(if (> (1- row1) 0) (setq dp3 (get_value (1- row1) col1 brd)) nil)
				(if (> (1- row1) 0) (setq dp4 (get_value (1- row1) col2 brd)) nil)
				
				(cond ((or (and (and (equal dp1 '-) (equal dp2 '-)) (and (equal dp3 '-) (equal dp4 '-)))
					(and (and (equal dp1 '--) (equal dp2 '--)) (and (equal dp3 '--) (equal dp4 '--)))) nil)
				      ((or (and (equal dp1 '-) (equal dp2 '-)) (and (equal dp1 '--) (equal dp2 '--)))
				      		(set_value1 (1+ row1) col1 fill_val brd) (set-sqr1 fill_val avl 0)
						(set_value1 (1+ row1) col2 (1+ fill_val) brd) (set-sqr1 (1+ fill_val) avl 0)
						(if (equal fill_op4 0)
							(setq fill_op4 1)
						nil)
				      )
				      ((or (and (equal dp3 '-) (equal dp4 '-)) (and (equal dp3 '--) (equal dp4 '--))) 
				      		(set_value1 (1- row1) col1 fill_val brd) (set-sqr1 fill_val avl 0)
						(set_value1 (1- row1) col2 (1+ fill_val) brd) (set-sqr1 (1+ fill_val) avl 0)
						(if (equal fill_op4 0)
							(setq fill_op4 1)
						nil)
				      )
				(t nil))	
		      )
		      
		      ((and (equal col2 col1) (or (equal row2 (1- row1)) (equal row2 (1+ row1))))
				(if (> (1- col1) 0) (setq dp1 (get_value row1 (1- col1) brd)) nil)
				(if (> (1- col1) 0) (setq dp2 (get_value row2 (1- col1) brd)) nil)
				(if (<= (1+ col1) n1) (setq dp3 (get_value row1 (1+ col1) brd)) nil)
				(if (<= (1+ col1) n1) (setq dp4 (get_value row2 (1+ col1) brd)) nil)
				
				(cond ((or (and (and (equal dp1 '-) (equal dp2 '-)) (and (equal dp3 '-) (equal dp4 '-))) 
					   (and (and (equal dp1 '--) (equal dp2 '--)) (and (equal dp3 '--) (equal dp4 '--)))) nil)
				      ((or (and (equal dp1 '-) (equal dp2 '-)) (and (equal dp1 '--) (equal dp2 '--)))
				      		(set_value1 row1 (1- col1) fill_val brd) (set-sqr1 fill_val avl 0)
						(set_value1 row2 (1- col1) (1+ fill_val) brd) (set-sqr1 (1+ fill_val) avl 0)
						(if (equal fill_op4 0)
							(setq fill_op4 1)
						nil)
				      )
				      ((or (and (equal dp3 '-) (equal dp4 '-)) (and (equal dp3 '--) (equal dp4 '--))) 
				      		(set_value1 row1 (1+ col1) fill_val brd) (set-sqr1 fill_val avl 0)
						(set_value1 row2 (1+ col1) (1+ fill_val) brd) (set-sqr1 (1+ fill_val) avl 0)
						(if (equal fill_op4 0)
							(setq fill_op4 1)
						nil)
				      )
				(t nil))	
		      )		
           
		(t nil))
	nil)		
)