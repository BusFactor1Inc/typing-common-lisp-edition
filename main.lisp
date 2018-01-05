;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;- Typing - Common Lisp Edition
;;
;;
;; Copyright BusFactor1 Inc. - 2017
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro halvef (x)
`(setf ,x (floor (/ ,x 2))))

(defmacro dividef (x n)
`(setf ,x (ceiling (/ ,x ,n))))

(defparameter *white*
  (color:make-rgb (/ 254 256.0) (/ 254 256.0) (/ 246 256.0)))
(defparameter *warm-blue*
  (color:make-rgb (/ 244 256.0) (/ 247 256.0) (/ 249 256.0)))
(defparameter *warm-red*
  (color:make-rgb (/ 253 256.0) (/ 64 256.0) (/ 35 256.0)))

(load "reschedule-timer.lisp") 
  
(defparameter +symbols+
  (sort (let (retval)
	  (do-symbols (s :cl)
		      (push s retval))
	  retval) #'<
	  :key (lambda (x) (length (symbol-name x)))))

(defparameter +nsymbols+ (length +symbols+))

(defparameter +symbols-2+ (remove-if (lambda (s) 
                                     (or (> 3 (length (symbol-name s)))
                                         (case (char (symbol-name s) 0)
                                           ((#\0 #\1 #\2 #\3 #\4 #\5
                                                 #\6 #\7 #\8 #\9)
					    t)
                                           (t nil))
                                         (< (random 1.0) .99)
                                         ))
				     +symbols+))

(defparameter +nsymbols-2+ (length +symbols-2+))
(defun random-word-2 ()
  (dotimes (tries 64 'the)
    (let ((word (nth
		 (random
		  (1+ (random
		       (min *difficulty* +nsymbols-2+))))
		 +symbols-2+)))
      
      (when (good-word (string-downcase
                          (symbol-name
                           word)))
	
        (return-from random-word-2 word)))))

(defparameter *font-1* (gp:make-font-description
                        :family "Monospace"
                        :size 32))

(defparameter *font-2* (gp:make-font-description
                        :family "Monospace"
                        :size 16))

(defparameter *font-3* (gp:make-font-description
                        :family "Monospace"
                        :size 64))

(defparameter *fonts* (make-array 128))
(dotimes (i 128)
  (setf (aref *fonts* i)
	(gp:make-font-description :family "Monospace" :size i)))

(defparameter *time-is-stopped* nil) 
(defparameter *frames* 0)
(defparameter *width* 900) 
(defparameter *height* (+ 124 (/ 1080 2)))
(defparameter *difficulty* 100) 
(defparameter *max-difficulty* 1000) 
(defparameter *score* 0) 
(defparameter *score-delta* 0) 
(defparameter *new-score* 0) 
(defparameter *flash* 0) 
(defparameter *flash-color* *warm-red*) 
(defparameter *num-correct-words* 0)
(defparameter *multiplier* 0)
(defparameter *new-multiplier* 1)
(defparameter *start-time* (get-universal-time))
(defparameter *current-time* (get-universal-time))
(defparameter *lives* 0)
(defparameter *show-start-screen* t)
(defparameter *press-any-key* -30)
(defparameter *mute* nil)

(defstruct current-character
  c x y (dx (+ -5 (random 10))) (dy (+ -10 (random 20))) 
  (ax (+ -1 (random 2)))
  (ay -2)
  (r 0)
  (dr (* .1 (+ -1 (random 3.0))))
  (size 64))

(defun sound-play (sound)
  (unless *mute*
    (capi:stop-sound sound)
    (capi:play-sound sound)))
  

(defun reset ()
  (setf *score* 0)
  (setf *new-score* 0)
  (setf *score-delta* 0)
  (setf *start-time* (get-universal-time))
  (setf *current-time* 0)
  (setf *multiplier* 0)
  (setf *new-multiplier* 0)
  (setf *flash* 0)
  (setf *lives* 3)
  (setf *difficulty* 100)
  (setf *typing-words* ()))

(defun filter (elt xs &key (test #'eql))
  (let (retval)
    (dolist (x xs)
      (when (not (funcall test x elt))
        (push x retval)))
    (nreverse retval)))

(defstruct word
  text typed-text x y dx ax (dy 0) (ay -1) width height shotgun (size (+ 32 (random 32))))

(defparameter *typing-words* ())

(defun get-text-width (pane text)
  (multiple-value-bind (left top right bottom)
      (gp:get-string-extent pane text)
    (declare (ignore top bottom))
    (- right left)))

(defun typing-get-y ()
  (flet ((geny ()
           (+ (* (floor (1+ (/ (random (- *height* 128)) 26))) 26) 28))
         (ytaken (y)
           (dolist (word *typing-words* nil)
             (when (= (word-y word) y)
               (return-from ytaken t)))))
    (dotimes (tries 24 -128) ; hide offscreen if there's too mcuh
                             ; already on screen, but then we get a
                             ; buildup and that's no good
      (let ((y (geny)))
        (unless (ytaken y)
          (return-from typing-get-y y))))))

(defun insert-word (pane get-word-function &optional (height 42))
  (let* (is-shotgun
         (text (funcall (if (not (= (random 33) 0))
                            get-word-function
                          (lambda () 
                            (setf is-shotgun (case (random 9)
                                               (0 :original)
                                               (1 :original)
                                               (2 :original)
                                               (3 :original)
                                               (4 :original)
                                               (5 :double-multiplier)
                                               (6 :double-multiplier)
                                               (7 :double-multiplier)
                                               (8 :plus-life)))
                            (string-downcase
                             (symbol-name (random-word-2))))))))
    (push (make-word :text text
                     :x (- (get-text-width pane text))
                     :y (typing-get-y)
                     :dx (1+ (random 4.0))
                     :width (get-text-width pane text)
                     :height height
                     :shotgun is-shotgun) 
          *typing-words*))
  (if (> (length *typing-words*) 1)
      (reschedule-timer *insert-word-timer*
			(* (random 1000) (/ 1 *difficulty*)))
    (reschedule-timer *insert-word-timer* 1)) ;
  (when (> .0001 *difficulty*)
    (decf *difficulty* (random 2))))

(defparameter *typing-timer* nil)
(defparameter *insert-word-timer* nil)
(defparameter *key-sound*
  (capi:read-sound-file "typewriter.wav")) 
(defparameter *fail-sound*
  (capi:read-sound-file "~fail.wav")) 
(defparameter *fail-2-sound*
  (capi:read-sound-file "~fail-2.wav"))
(defparameter *shotgun-sound*
  (capi:read-sound-file "shotgun.wav"))
(defparameter *bing-sound*
  (capi:read-sound-file "bing.wav"))
(defparameter *time-is-stopped-sound*
  (capi:read-sound-file "time-is-stopped.wav"))
(defparameter *plus-life-sound*
  (capi:read-sound-file "plus-life.wav"))
(defparameter *half-multiplier-sound*
  (capi:read-sound-file "half-multiplier.wav"))
(defparameter *double-multiplier-sound*
  (capi:read-sound-file "double-multiplier.wav"))
(defparameter *noise-hit-sound*
  (capi:read-sound-file "noise-hit.wav"))
(defparameter *110-hit-sound*
  (capi:read-sound-file "110-hit.wav"))
(defparameter *220-hit-sound*
  (capi:read-sound-file "220-hit.wav"))
(defparameter *440-hit-sound*
  (capi:read-sound-file "440-hit.wav"))
(defparameter *880-hit-sound*
  (capi:read-sound-file "880-hit.wav"))
(defparameter *next-hit-sound*
  (capi:read-sound-file "next-hit.wav"))
(defparameter *next-hit-2-sound*
  (capi:read-sound-file "next-hit-2.wav"))
(defparameter *chirp-sound*
  (capi:read-sound-file "chirp.wav"))
(defparameter *square-bass-sound*
  (capi:read-sound-file "square-bass.wav"))
(defparameter *square-bass-2-sound*
  (capi:read-sound-file "square-bass-2.wav"))
(defparameter *bell-sound*
  (capi:read-sound-file "bell.wav"))

(defun good-word (word)
  (let ((first-char (char word 0))
        not-good)
    (dolist (word-2 *typing-words*)
      (let ((first-char-2 (char (word-text word-2) 0)))
        (setf not-good (or (char= first-char first-char-2) not-good))))
    (not not-good)))

(defun get-word ()
  (flet ((random-word ()
           (string-downcase
            (symbol-name
             (nth (floor (random *difficulty*)) +symbols+)))))
    (incf *difficulty*)
    (dotimes (tries (max 1 (- 1000 *difficulty*)) (string-downcase (symbol-name (random-word-2))))
      (let ((word (random-word)))
        (when (good-word word)
          (return-from get-word word))))))


(defun draw-border (pane)
  ;; borders
  (gp:set-graphics-state pane
                         :foreground :black)
  (gp:draw-line pane 0 1 *width* 1) ; _
        (gp:set-graphics-state pane
                               :foreground :lightgrey)
  (gp:draw-line pane 0 (1- *height*) *width* (1- *height*)) ; _
  (gp:draw-line pane 7 24 (- *width* 7) 24 :thickness 4) ; _
  (gp:draw-line
   pane 7 (- *height* 7) (- *width* 7) (- *height* 7) :thickness 4) ; _
  (gp:draw-line pane 1 0 1 *height*) ; | ;
  (gp:draw-line pane (1- *width*) 0 (1- *width*) *height*) ; |
  (gp:draw-line pane 8 23 8 (- *height* 6) :thickness 4) ; |
  (gp:draw-line
   pane (- *width* 6) 23 (- *width* 8) (- *height* 6) :thickness 4) ; |

  (gp:draw-rectangle pane 8 6 12 12)

  (gp:draw-line pane 24 6 (- *width* 48) 6)
  (gp:draw-line pane 24 10 (- *width* 48) 10)
  (gp:draw-line pane 24 14 (- *width* 48) 14)
  (gp:draw-line pane 24 18 (- *width* 48) 18)

  (gp:draw-rectangle pane (- *width* 44) 6 13 13)
  (gp:draw-rectangle pane (- *width* 44) 6 6 6)

  (gp:draw-rectangle pane (- *width* 24) 6 13 6)
  (gp:draw-rectangle pane (- *width* 24) 13 13 6)
)

(defun draw-status-bar (pane &optional (font *font-1*))
  (gp:set-graphics-state pane
                         :foreground *warm-blue*)

  (gp:draw-rectangle pane 0 (- *height* 48) *width* 48 :filled t)
      

  (gp:set-graphics-state pane
                         :foreground :gray)

  (gp:draw-line pane 0 (- *height* 48) *width* (- *height* 48))
      
  (gp:set-graphics-state pane
                         :foreground :black
                         :font (gp:find-best-font pane
                                                  font))
  (gp:draw-string pane
                  (format nil "Score: ~A" *score*)
                  16
                  (- *height* 14))

  (if (<= *new-multiplier* *multiplier*)
      (unless (= *new-multiplier* *multiplier*)
        (decf *multiplier*))
    (incf *multiplier*))
  
  (gp:draw-string pane
                  (format nil "Multiplier: ~A" *multiplier*)
                  260
                  (- *height* 14))
      
  (when (> *lives* 0)
    (setf *current-time* (get-universal-time)))
      
  (gp:draw-string pane
                  (format nil "Time: ~A" (- *current-time* *start-time*))
                  540
                  (- *height* 14))
      
  (gp:draw-string pane
                  (format nil "Lives: ~A" *lives*)
                  (- *width* 128)
                  (- *height* 14)))

(defun random-color ()
  (case (random 11)
    (0 :gray)
    (1 :blue)
    (2 :green)
    (3 :white)
    (4 :black)
    (5 :tan)
    (6 :cyan)
    (7 :pink)
    (8 :yellow)
    (9 *warm-red*)
    (10 :orange)))

(defun draw-start-screen (pane)
  (gp:set-graphics-state pane
                         :foreground :black
                         :font (gp:find-best-font 
                                pane *font-1*))
  
  (if (= (random 47) 0)
      (insert-word pane #'get-word))
  
  (gp:draw-rectangle pane
                     200 200
                     500 300
                     :foreground *warm-blue*
                     :filled t)
  
  (gp:draw-rectangle pane
                     200 200
                     500 300)
  
  (gp:draw-line pane
                     200 224
                     700 224)
  
  (gp:draw-string pane
                  "Typing - Common Lisp Edition"
                  (- (/ *width* 2)
		     (/ (get-text-width
			 pane "Typing - Common Lisp Edition") 2))
                  (- (/ *height* 2) 60))

  (if (> *press-any-key* 0)
      (progn
        (decf *press-any-key*)
        (gp:draw-string pane
                        "Press Space Bar To Play"
                        (- (/ *width* 2)
			   (/ (get-text-width
			       pane "Press Space Bar To Play") 2))
                        (+ (/ *height* 2) 4)))
    (when (eq (mod (get-universal-time) 2) 0)
      (setf *press-any-key* 30)))
  
  (gp:set-graphics-state pane
                         :foreground :black
                         :font (gp:find-best-font pane
                                                  *font-2*))
  (gp:draw-string pane
                  "BusFactor1 Inc. - 2017"
                  (- (/ *width* 2)
		     (/ (get-text-width
			 pane "BusFactor1 Inc. - 2017") 2))
                  (+ (/ *height* 2) 155))

  (gp:set-graphics-state pane
                         :foreground :black
                         :font (gp:find-best-font pane
                                                  *font-3*))
  (gp:draw-rectangle pane
                  (- (/ *width* 2) (/ (get-text-width pane "[1]") 2) 4)
                  (+ (/ *height* 2) 46)
                  78 74 :foreground :black :thickness 4)

  (gp:draw-rectangle pane
                  (- (/ *width* 2) (/ (get-text-width pane "[1]") 2) 2)
                  (+ (/ *height* 2) 48)
                  74 70 :foreground *warm-red* :filled t)

  (gp:draw-string pane
                  "[  ]"
                  (- (/ *width* 2) (/ (get-text-width pane "[  ]") 2))
                  (+ (/ *height* 2) 100))

  (gp:draw-string pane
                  "1"
                  (- (/ *width* 2) (/ (get-text-width pane "1") 2) 0)
                  (+ (/ *height* 2) 104))

  
  (gp:set-graphics-state pane
                         :foreground :black
                         :font (gp:find-best-font pane
                                                  *font-1*))
  )

(defun draw-game-over-screen (pane)
  (gp:set-graphics-state pane
                         :foreground :black)
  (gp:draw-rectangle
   pane 220 200 460 300 :foreground *warm-blue* :filled t)
  (gp:draw-rectangle pane 220 200 460 300)
  (gp:draw-line pane 220 220 680 220)
  
  (gp:draw-string pane
                  "Game Over"
                  (- (/ *width* 2)
                     (/ (get-text-width paNe "Game Over") 2))
                  (- (/ *height* 2) 46))
  
  (let ((score-string
         (format nil "Final Score: ~A PPS"
                 (floor (/ *score* 
                           (- *current-time* 
                              *start-time*))))))
    
    (gp:draw-string pane
                    score-string
                    (- (/ *width* 2)
                       (/ (get-text-width paNe score-string) 2))
                    (+ (/ *height* 2) 10)))
  
  (if (> *press-any-key* 0)
      (progn
        (decf *press-any-key*)
        (gp:draw-string pane
                        "Press Space Bar To Play"
                        (- (/ *width* 2)
                           (/ (get-text-width
                               pane "Press Space Bar To Play") 2))
                        (+ (/ *height* 2) 100)))
    (when (eq (mod (get-universal-time) 2) 0)
      (setf *press-any-key* 30))))


(defun typing-display-callback (pane &rest rest)
  (declare (ignore rest))
  (capi:with-atomic-redisplay (pane)
    (if (and (>= (decf *flash*) 0)
             (= 0 (random 2)))
        (gp:set-graphics-state pane
                               :foreground (case *flash-color*
                                             (:random (random-color))
                                             (t *flash-color*)))
          (gp:set-graphics-state pane
                                 :foreground *white*))
      
    (gp:draw-rectangle pane 0 0  *width* *height* :filled t)

    (gp:set-graphics-state pane
                           :foreground :black)

    (incf *score* (floor (/ (- *new-score* *score*) 2)))

    (if (= *lives* 0)
        (progn
          (gp:set-graphics-state pane :foreground :black)
          (dolist (word *typing-words*)
            (if (zerop (length (word-typed-text word)))
                (progn
                  (if (word-shotgun word)
                      (gp:set-graphics-state pane
                                             :foreground (random-color))
                    (gp:set-graphics-state pane
                                           :foreground :black))
                  (gp:draw-string pane
                                  (word-text word) 
                                  (word-x word)
                                  (word-y word)))
              (progn
                (gp:set-graphics-state pane
                                       :foreground (if (word-shotgun word) 
                                                       (random-color)
                                                     *warm-red*))
                (gp:draw-string pane
                                (word-typed-text word) 
                                (word-x word)
                                (word-y word))
                (gp:set-graphics-state pane
                                       :foreground :black)))
            (unless *time-is-stopped*
              (incf (word-x word) (word-dx word))))
          
          (setf *typing-words*
                (remove-if #'null
                           (mapcar (lambda (word)
                                     (if (> (word-x word) *width*)
                                         nil word)) *typing-words*)))
          (if *show-start-screen*
              (draw-start-screen pane)
            (draw-game-over-screen pane)))

      (catch 'die
        (when *current-characters*
          (gp:set-graphics-state
	   pane :foreground (case (random 2) (0 *warm-red*) (1 :yellow)))
          (setf *current-characters* 
                (remove-if
                 (lambda (cc)
                   (or (<= (current-character-x cc) 0)
                       (> (current-character-y cc) (- *height* 42))))
                 (let ((i -1))
                   (mapcar (lambda (cc)
                             (gp:set-graphics-state 
                              pane :font (gp:find-best-font 
                                          pane
                                          (aref *fonts* 
                                                (dividef
                                                 (current-character-size cc) 1.05))))
                             (gp:with-graphics-translation (pane 
                                                            (current-character-x cc)
                                                            (current-character-y cc))
                               (gp:with-graphics-rotation (pane (incf (current-character-r cc) 
                                                                      (current-character-dr cc)))
                                 (gp:with-graphics-translation (pane 
                                                                (- (current-character-x cc))
                                                                (- (current-character-y cc)))
                                 (gp:draw-string pane
                                                 (current-character-c cc) 
                                                 (current-character-x cc)
                                                 (current-character-y cc)))))
                               (decf (current-character-x cc)
                                     (current-character-dx cc))
                               (incf (current-character-dx cc)
                                     (current-character-ax cc))
                               (decf (current-character-y cc)
                                     (current-character-dy cc))
                               (incf (current-character-dy cc)
                                     (current-character-ay cc))
                               cc)
                           *current-characters*)))))
        
        (gp:set-graphics-state pane :foreground :black :font (gp:find-best-font pane *font-1*))
        (dolist (word *typing-words*)
          (if (zerop (length (word-typed-text word)))
              (progn
                (if (word-shotgun word)
                    (gp:set-graphics-state pane
                                           :foreground (random-color))
                  (gp:set-graphics-state pane
                                         :foreground :black))
                (gp:draw-string pane
                                (word-text word) 
                                (word-x word)
                                (word-y word)))
            (progn
              (gp:set-graphics-state pane
                                     :foreground (if (word-shotgun word) 
                                                     (random-color)
                                                   *warm-red*))
              (gp:draw-string pane
                              (word-typed-text word) 
                              (word-x word)
                              (word-y word))
              (gp:set-graphics-state pane
                                     :foreground :black)))

          (unless *time-is-stopped*
            (incf (word-x word) (word-dx word)))
          
          (when (> (+ (word-x word) (word-width word)) *width*)
            (die word)))
        
        (gp:set-graphics-state pane
                               :foreground :grey)
        #|(setf *falling-words*
	      (remove-if #'null
			 (mapcar (lambda (word)
				   (incf (word-dy word) (word-ay word))
				   (incf (word-y word) (word-dy word))
				   (incf (word-dx word) (word-ax word))
				   (incf (word-x word) (word-dx word))
				   (if (or (< (word-y word) 36)
					   (> (word-y word) *height*)
					   (< (word-x word) 0)
					   (> (word-x word) *width*))
				       nil
				     (progn
				       (gp:draw-string pane
						       (word-text word) 
						       (word-x word)
						       (word-y word))
				       word)
				     ))
				 *falling-words*)))|#
        
    ))
    (draw-status-bar pane)

    (incf *frames*)
    (when (and (< (random 1.0) .50) (zerop (mod *frames* 3))) ;
      (case (random 6)
        (0 (sound-play *next-hit-2-sound*))
        (1 (sound-play *220-hit-sound*))
        (2 (sound-play *440-hit-sound*))
        (3 (sound-play *880-hit-sound*))
        (4 (sound-play *880-hit-sound*)) ;
        (5 (sound-play *square-bass-2-sound*))))
    (when (and (< (random 1.0) .25) (zerop (mod *frames* 12)))
      (case (random 6) ;
        (0 (sound-play *next-hit-sound*))
        (1 (sound-play *220-hit-sound*))
        (2 (sound-play *440-hit-sound*))
        (3 (sound-play *880-hit-sound*))
        (4 (sound-play *noise-hit-sound*))
        (5 (sound-play *chirp-sound*))))
    (when (zerop (mod *frames* 12))
      (when (< (random 1.0) .15) (sound-play *square-bass-sound*))
      (sound-play *110-hit-sound*))
    (when (eq 6 (mod *frames* 24))
      (sound-play *noise-hit-sound*))
    ) ; | ;

    
  (reschedule-timer *typing-timer* (/ 1 30)))

(defparameter *current-word* ())
(defparameter *current-characters* ())
(defun die (word)
  (declare (ignore word))
  (sound-play *fail-sound*)
  (setf *score-delta* 0)
  (dolist (word *typing-words*) ;; subtract value of all the words on
				;; the screen from score
    (decf *score-delta*
	  (* *new-multiplier* (length (word-text word)))))
  (incf *new-score* *score-delta*)
  (setf *typing-words* nil)
  (setf *current-word* nil) ;
  (setf *current-characters* nil)
  (setf *new-multiplier* 0) 
  (reschedule-timer *insert-word-timer* 0)
  (decf *lives*)
  (setf *flash* 10)
  (setf *flash-color* :black)
  (throw 'die nil))

(defun typing-handle-character (pane x y character)
  (declare (ignore x y)) 
  (flet ((success ()
           ;; successfully typed the next character of the word
           (push (make-current-character :c (string-downcase (format nil "~A" character))
                                         :x (word-x *current-word*)
                                         :y (word-y *current-word*))
               *current-characters*)
           (sound-play *key-sound*)
           (setf *score-delta* 1)
           (incf *new-score* *score-delta*)
           (when (> (length (word-typed-text *current-word*))  0)
             (setf (word-typed-text *current-word*) 
                   (subseq (word-typed-text *current-word*) 1)))
           (setf (word-width *current-word*)
		 (get-text-width pane 
				 (word-typed-text *current-word*)))
           (when (zerop (length (word-typed-text *current-word*)))
             (if (word-shotgun *current-word*)
                 (let ((sum 0))
                   (case (word-shotgun *current-word*)
                     (:double-multiplier
                      (sound-play *double-multiplier-sound*)
                      (setf *new-multiplier* 
                            (* *new-multiplier* 2)))
                      
                     (:plus-life 
                      (sound-play *plus-life-sound*)
                      (incf *lives*))
                     (t
                      (sound-play *shotgun-sound*)))
                   (when (eq (word-shotgun *current-word*) :original)
                       (dolist (word *typing-words*)
                         (incf sum (length (word-text word)))

                         (dotimes (i (length (word-text word)))
                           (push (make-current-character :c (string-downcase
                                                             (format nil "~A" 
                                                                     (aref (word-text word) 
                                                                           i)))
                                                         :x (incf (word-x word) 
                                                                  (random 22))
                                                         :y (word-y word))
                                 *current-characters*)))
                       (setf *score-delta* (* *new-multiplier* sum))
                       (incf *new-score* *score-delta*)
                       (setf *flash* 27)
                       (setf *flash-color* :random)
                       (setf *typing-words* ())))
               (progn
                 (sound-play *bell-sound*)
                 (setf *score-delta* (* (1+ *new-multiplier*)
                                           (length (word-text *current-word*))))
                 (incf *new-score* *score-delta*)))
             (incf *num-correct-words*)
             (incf *new-multiplier*)
             (setf *typing-words*
		   (filter *current-word* *typing-words* :test #'equalp))
             (reschedule-timer *insert-word-timer* 0)
             (setf *current-word* nil))))
    (if (eq *lives* 0)
        (progn
          (when (char= #\Space character)
            (setf *show-start-screen* nil)
            (reset)
            (unless *typing-words* (insert-word pane #'get-word))))
      (if *current-word*
          (if (and (not (char= character #\Escape))
                   (and (> (length (word-typed-text *current-word*)) 0)
                        (char= (char (word-typed-text *current-word*) 0)
			       character)))
              (success)
            ;; mistyped the word, reset
            (progn
              (sound-play *fail-2-sound*)
              (setf *new-multiplier* 0)
           (setf *score-delta* (- (* (1+ *new-multiplier*)
                                     (length (word-text *current-word*)))))
           (incf *new-score* *score-delta*)
           (setf *flash* 10)
           (setf *flash-color* *warm-red*)
           (setf (word-typed-text *current-word*) "")
           (setf (word-width *current-word*)
		 (get-text-width pane (word-text *current-word*)))
           (setf *current-word* nil)))
        ;; no current word, so see if they hit a new one
        (dolist (word *typing-words*)
          (when (and (> (length (word-text word)) 0)
                     (char= (char (word-text word) 0) character)
                     (> (word-x word) 8))
            (setf *score-delta* 1)
            (incf *new-score* *score-delta*)
            (sound-play *key-sound*)
            (setf *current-word* word)
            (setf (word-typed-text word) (subseq (word-text word) 1))
            (push (make-current-character :c (string-downcase (format nil "~A" character))
                                          :x (word-x *current-word*)
                                          :y (word-y *current-word*))
                  *current-characters*)
            (when (= (length (word-text word)) 1)
              (success))
            (return-from typing-handle-character nil)))))))

(defun main-display-callback (&rest args)
  (if *in-intro*
      (apply #'intro-display-callback args)
    (apply #'typing-display-callback  args)))

(defun main-handle-character (&rest args)
  (if *in-intro* ;
      (apply #'intro-handle-character args)
    (apply #'typing-handle-character args)))

(capi:define-interface my-interface ()
  ()
  (:panes
   (main capi:output-pane
         :accessor main-pane
         :display-callback 'main-display-callback
         :background *white*
         :input-model '((:character main-handle-character))
         :initial-constraints
         `(:visible-min-width ,*width* 
           :visible-min-height ,*height*))))


(defparameter *in-intro* t)
(defparameter *intro-frames* 0)
(defparameter *intro-timer* nil)
(defparameter *intro-magic-1* 0)
(defparameter *intro-magic-2* 30)
(defparameter *intro-played-bell* nil)

(defun intro-display-callback (pane &rest args)
  (incf *intro-frames*)
  
  (capi:with-atomic-redisplay (pane)
    (gp:draw-rectangle pane 0 0 *width* *height*
                       :foreground *white* :filled t))
  
  (when (zerop *intro-magic-1*)
    (setf *intro-magic-1* *intro-magic-2*)
    (dividef *intro-magic-2* 2)) ;

  (decf *intro-magic-1*)

  (when (= 0 *intro-magic-1*)
    (gp:set-graphics-state pane
                           :foreground :black
                           :font (gp:find-best-font pane
                                                    (aref *fonts* 120)))

    (if (< *intro-frames* (* 30 3))
        (progn
          (gp:draw-string pane
                          (format nil " ~A " (random 9))
                          (/ *width* 2) (/ *height* 2))
          (sound-play *bing-sound*))
      (progn
        (gp:draw-string pane "[1]"
                        (/ *width* 2) (/ *height* 2))
        (unless *intro-played-bell*
          (sound-play *bell-sound*)
          (setf *intro-played-bell* t)))))
    
  (when (= *intro-frames* (* 30 7))
    (setf *in-intro* nil))

  (reschedule-timer *intro-timer* (/ 1 30)))

(defun intro-handle-character (pane x y character)
  (print character)
  (setf *in-intro* nil))

   
(defun main ()
  (setf *current-word* nil)
  (setf *typing-words* nil)

  (setf *key-sound* (capi:load-sound *key-sound*))
  (setf *fail-sound* (capi:load-sound *fail-sound*))
  (setf *fail-2-sound* (capi:load-sound *fail-2-sound*))
  (setf *shotgun-sound* (capi:load-sound *shotgun-sound*))
  (setf *time-is-stopped-sound* (capi:load-sound *time-is-stopped-sound*))
  (setf *plus-life-sound* (capi:load-sound *plus-life-sound*))
  (setf *bell-sound* (capi:load-sound *bell-sound*))
  (setf *bing-sound* (capi:load-sound *bing-sound*))
  (setf *half-multiplier-sound* (capi:load-sound *half-multiplier-sound*))
  (setf *double-multiplier-sound* (capi:load-sound *double-multiplier-sound*))
  (setf *noise-hit-sound* (capi:load-sound *noise-hit-sound*))
  (setf *110-hit-sound* (capi:load-sound *110-hit-sound*))
  (setf *220-hit-sound* (capi:load-sound *220-hit-sound*))
  (setf *440-hit-sound* (capi:load-sound *440-hit-sound*)) 
  (setf *880-hit-sound* (capi:load-sound *880-hit-sound*))
  (setf *next-hit-sound* (capi:load-sound *next-hit-sound*)) ;
  (setf *next-hit-2-sound* (capi:load-sound *next-hit-2-sound*))
  (setf *chirp-sound* (capi:load-sound *chirp-sound*))
  (setf *square-bass-sound* (capi:load-sound *square-bass-sound*))
  (setf *square-bass-2-sound* (capi:load-sound *square-bass-2-sound*))

  

    (let* ((interface (capi:display 
                       (make-instance 
                        'my-interface
                        :Title "Typing - Common Lisp Edition by BusFactor1 Inc.")))
           (pane (main-pane interface)))

      (capi:display interface)

      (multiple-value-bind (w h) (capi:simple-pane-visible-size pane)
        (setf *height* h)
        (setf *width* w))

      (setf *typing-timer* (mp:make-timer #'gp:invalidate-rectangle pane))
      (setf *intro-timer* *typing-timer*)
      (setf *insert-word-timer* (mp:make-timer #'insert-word pane #'get-word))
      
      (gp:set-graphics-state pane
                             :font (gp:find-best-font pane
                                                      *font-1*))
      (reschedule-timer *typing-timer* (/ 1 30))
      (reschedule-timer *insert-word-timer* 1)))
