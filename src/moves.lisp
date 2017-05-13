;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Функції повязані з ходами на пустій дошці і перевірці коректності
;;;;
;;;;


(in-package :ccs)


;;; Ходи на пустій дошці

(defgeneric moves (piece start &key color)
  (:documentation "Повертає всі можливі ходи (список полів) фігури PIECE кольору COLOR з поля START на пустій дошці."))

(defgeneric pre-moves (piece to &key color attack-only)
  (:documentation "Find all squares where a PIECE should be placed in order to reach TO field in one move."))

(defmethod moves ((piece (eql :rook)) start &key (color :white))
  "Rachable squares by rook тура"
  (declare (ignorable color))
  (loop :for p :from 1 :to 64
     :when (and
	    (not (= p start))
	    (or
	     (eql (mod p 8) (mod start 8))
	     (eql (ceiling p 8) (ceiling start 8))))
     :collecting p))

(defmethod moves ((piece (eql :bishop)) start &key (color :white))
  "Reachable squares by bishop"
  (declare (ignorable color))
  (loop :for p :from 1 :to 64
     :when (and
	    (not (= p start))
	    (eql (abs (- (mod (1- start) 8) (mod (1- p) 8)))
		 (abs (- (ceiling start 8) (ceiling p 8)))))
     :collecting p))


(defmethod moves ((piece (eql :queen)) start &key (color :white))
  (declare (ignorable color))
  (union (moves :rook start) (moves :bishop start)))


(defmethod moves ((piece (eql :king)) start &key (color :white))
  (loop :for p :from 1 :to 64
     :when (or
	    ;; хід на одне поле
	    (and
	     (<= (abs (- (mod (1- p) 8) (mod (1- start) 8))) 1)
	     (<= (abs (- (ceiling p 8) (ceiling start 8))) 1)
	     (not (= p start)))
	    ;; рокіровка
	    (and (= start (if (eql color :white) #@e1@ #@e8@))
		 (= (abs (- p start)) 2)))
     :collecting p))


(defmethod moves ((piece (eql :knight)) start &key (color :white))
  (declare (ignore color))
  (loop :for p :from 1 :to 64
     :when (equal
	   (sort (list
		  (abs (- (mod (1- p) 8) (mod (1- start) 8)))
		  (abs (- (ceiling p 8) (ceiling start 8))))
		 #'<=)
	   '(1 2))
     :collecting p))


(defmethod moves ((piece (eql :pawn)) start &key (color :white))
  (let (res)
    (when (and (eql color :white) (< (ceiling start 8) 8))
      (push (+ start 8) res)
      (when (not (= (mod start 8) 1)) (push (+ start 7) res))
      (when (not (= (mod start 8) 0)) (push (+ start 9) res))
      (when (= (ceiling start 8) 2) (push (+ start 16) res)))
    (when (and (eql color :black) (> (ceiling start 8) 1))
      (push (- start 8) res)
      (when (not (= (mod start 8) 1)) (push (- start 9) res))
      (when (not (= (mod start 8) 0)) (push (- start 7) res))
      (when (= (ceiling start 8) 7) (push (- start 16) res)))
    (delete-if-not #'(lambda (sq) (and (<= 1 sq) (<= sq 64))) res)))


(defmethod moves ((piece piece) start &key (color :white))
  (declare (ignore color))
  (moves (kind piece) start :color (color piece)))


(defmethod pre-moves ((piece piece) to &key (color nil) (attack-only nil))
  (case (kind piece)
    (:pawn
       (loop :for sq :from 1 :to 64
          :when (and (member to (moves piece sq :color (or color (color piece))))
                     (or (null attack-only)
                         (/= (mod to 8) (mod sq 8))))
          :collect sq))
    (t (moves (kind piece) to :color (or color (color piece))))))

;;; Пошук можливих траєкторій руху фігури на пустій дошці

(fare-memoization:define-memo-function find-paths (piece start end horizon &key (color :white))
  "Return: list of paths; each path is a list of squares."
  (let ((wave `((,start)))
	(result nil))
    ;; generate all paths
    (loop :while (< 0 horizon)
       :do (decf horizon)
	 (setq
	  wave
	  (apply #'nconc
		 (mapcar ; append possible steps to each path
		  #'(lambda (path)
		      (mapcar
		       #'(lambda (next) (append path (list next)))
		       (remove-if ; no loops allowed
			#'(lambda (next) (member next path))
			(moves piece (car (last path)) :color color))))
		  wave)))
	 (setq wave (delete-duplicates wave :test #'equal))
	 (setq result (append result wave)))
    ;; filter out all paths not leading to end
    (delete-if-not
     #'(lambda (p) (eql (car (last p)) end))
     result)))

(defun path-to-moves (path)
  "Split PATH into list of consecutive moves, e.g. CONSes of
squares. (1 2 3) => ((1 . 2) (2 . 3))"
  (loop :for tail :on path
     :when (cdr tail) :collect (cons (first tail) (second tail))))


;;; Перевірка допустимості ходу

(defun squares-on-line (start end &key (include-frontier t))
  "Список полів між START і END (вертикаль, горизонталь або діагональ), включаючи FROM і TO."
  (let ((from (min start end))
	(to (max start end))
	offset path)
    (setf offset
	  (cond
	    ((eql (mod from 8) (mod to 8)) 8) ; vertical
	    ((eql (floor (1- from) 8) (floor (1- to) 8)) 1) ; line
	    ((eql (abs (- (mod (1- from) 8) (mod (1- to) 8))) ; diagonal
		  (abs (- (ceiling from 8) (ceiling to 8))))
	     (+ 8 (signum (- (mod (1- to) 8) (mod (1- from) 8)))))
	    (t (return-from squares-on-line nil))))
    (do ((s from (+ s offset)))
	((> s to))
      (when (or include-frontier (and (/= s from) (/= s to))) (push s path)))
    (if (< start end)
        (nreverse path)
        path)))



(defun free-line-p (board from to)
  "Перевірка того, що на лінії між FROM і TO немає фігур. Зайнятість полів FROM і TO не враховується. Повертається nil, якщо FROM і ТО не знаходятся на одній прямій."
  (let ((from (min from to))
	(to (max from to))
	line)
    (setf line (squares-on-line from to))
    (if line
	(every #'(lambda (sq) (or (member sq (list from to))
				  (null (whos-at board sq))))
		 line)
	nil #| no line between from and to |# )))


(defun castling-move-p (piece color move)
  "Перевірка того, що хід є рокіровкою."
  (let ((offset (if (eql color :white) 0 56)))
    (and
     (eql (kind piece) :king)
     (eql (move-from move) (+ #@e1@ offset))
     (member (move-to move) (list (+ #@c1@ offset)
				  (+ #@g1@ offset))))))


(defun pawn-take-move-p (piece color move)
  "Перевірка того, що хід є взяття пішаком."
  (let ((from (move-from move))
	(to (move-to move)))
    (and
     (eql (kind piece) :pawn)
     (member to (moves piece from :color color))
     (not (= (mod from 8) (mod to 8)))))) ; пішак міняє вертикаль


(defun attackers (board to &key (from nil) (color nil) (attack-only t) (free-line t))
  "Повертає список полів на яких стояит фігури кольору COLOR, які атакують
   в позиції BOARD поле TO. Якщо COLOR не задано, то видаються фігури обох
   кольорів. Якщо задано поле FROM, то перевіряється лише ця фігура.
   Якщо ATTACK-ONLY = nil, то додаються рокіровка та ходи пішаками по прямій.
   FREE-LINE nil відключає перевірку того, що траєкторія руху вільна.
   "
  (check-type board board)
  (let (res)
    (loop :for from-sq :from (or from 1) :to (or from 64) :do
       (multiple-value-bind (p c) (whos-at board from-sq)
         (when (and (not (null p)) ; на поле from є фігура
                    (or (not color) (eql color c)) ; потрібного кольору
                    (member to (moves p from-sq :color c)) ; фігура може потрапити на to
                    (or (not free-line) ; перевірка не потрібна
                        (eql (kind p) :knight)
			(free-line-p board from-sq to)) ; вільні поля
                    (or (not attack-only)
                        (and ; додаткові умови якщо лише напад
                            (or (not (eql (kind p) :pawn))
                             (not (= (mod from-sq 8) (mod to 8)))) ; пішак міняє вертикаль
                         (not (castling-move-p p c (create-move from-sq to)))))) ; не рокіровка
           (push from-sq res))))
    res))


(defun check-p (board color)
  "Перевірка того, що є шах королю кольору COLOR."
  (let ((k-square (first (find-pieces board :king color))))
    (attackers board k-square :color (opposite-color color))))


(defun valid-move-p (board move &key (valid-turn nil))
  (declare (board board) (move move))
  "Перевірка допустимості ходу MOVE. Якщо VALID-TURN != NIL, то перевіряється що на полі from стоїть фігура того кольору, чий хід зараз."
  (let ((from (move-from move))
	(to (move-to move)))
  (multiple-value-bind (piece color) (whos-at board from)
    (multiple-value-bind (piece-to color-to) (whos-at board to)
      (when (and (or (null valid-turn) (eql color (turn board))) ; наша фігура на полі from
		 (attackers board to :from from :color color :attack-only nil) ; може потрапити на поле to
		 (not (eql color color-to)) ; на поле to нет нашей фигуры
		 (not (check-p (make-move (copy-board board) move) color))) ; посля ходу нам немає шаху
    ;; основні умови виконані;
	;; додаткові
	(cond
	  ((eql (kind piece) :pawn)
	   (if (pawn-take-move-p piece color move)
	       (when
		   (not (or ; умова взяття пішака
			 (eql color-to (opposite-color color)) ; є хтось
			 (eql to (enpassant board)))) ; можливе взяття на проході
		 (return-from valid-move-p nil))
	       ;; це не взяття - поле to має бути вільне
	       (when (not (null piece-to))
		 (return-from valid-move-p nil))))
	  ((eql (kind piece) :king)
	   ;; уомва неможливості  рокіровки
	   (when (castling-move-p piece color move)
	     (when (or
		    (not (member to (castlings board))) ; хтось ходив
		    (not (free-line-p board from (if (< to from) (- to 2) (+ to 1)))) ; зайняте b1, g1
		    ;; шах або бите поле
		    (some (complement #'null)
			  (mapcar #'(lambda (sq)
				      (attackers board sq :color (opposite-color color)))
				  (squares-on-line from to))))
	       (return-from valid-move-p nil)))))
	;; все виконано
	(return-from valid-move-p t))
    nil))))
