;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;

(in-package :ccs)

(defclass piece ()
  ((color :type color :accessor color :initarg :color)
   (kind :type piece-kind :accessor kind :initarg :kind :documentation "Тип фігури."))
  (:documentation "Базовий клас для всіх фігур."))


(defclass board ()
  ((pieces
    :initform (make-array 65)
    :accessor pieces
    :documentation "Масив обєктів типу piece. Якщо поле вільне, то елемент = nil. Нульвий елемент масиву не використовується.")
   (turn
    :type color :initform :white
    :accessor turn
    :documentation "Хто ходить наступний")
   (castlings
    :accessor castlings
    :documentation "Які рокіровки дозволені. Список c1,g1,c8,g8")
   (enpassant
    :accessor enpassant
    :documentation "Клітинки де можливе взяття на проході"))
  (:documentation "Опис положення фігур на дошці."))


(defun copy-board (board)
  "Створює копію обєкту board."
  (let ((copy (shallow-copy-object board)))
    (setf (pieces copy) (copy-seq (pieces board)))
    copy))

(defun create-board () (make-instance 'board))


(defgeneric whos-at (board sq)
  (:documentation "Повертає для поля SQ фігуру та її колір (два значення) або nil.")
  (:method ((board board) sq)
    (let ((w (aref (pieces board) sq)))
      (if w (values w (color w)) nil ))))


(defun empty-square-p (board square)
  (null (whos-at board square)))

(defgeneric load-from-fen-string (board string)
  (:documentation "Ініціалізує позицію з рядка в нотації FEN."))


(defun piece-to-name (piece)
  (car (rassoc piece
	      '((#\K . :king) (#\Q . :queen) (#\R . :rook)
		(#\N . :knight) (#\B . :bishop) (#\P . :pawn)))))


(defgeneric print-board (board &key stream)
  (:documentation "Виводить теперішнє положення в стандартній нотації.")
  (:method ((board board) &key (stream t))
    (format stream "~A"
            (with-output-to-string (s)
              (mapcar
               #'(lambda (color)
                   (loop
                      :for sq :from 1 :to 64 :do
                      (when (= sq 1) (format s "~A " (string color)))
                      (let ((pc (aref (slot-value board 'pieces) sq)))
                        (when (and pc (eql (color pc) color))
                          (format s "~C~A "
                                  (piece-to-name (kind pc))
                                  (square-to-string sq))))))
               '(:white :black))))))


(defgeneric print-board-fen (board &key stream)
  (:documentation "Виводить теперішнє положення в форматі FEN.")
  (:method ((board board) &key (stream t) )
    (print-board board :stream stream)))


(defgeneric find-pieces (board piece color)
  (:documentation "Знаходить поля на яких стоять фігури типу PIECE кольору COLOR. Якщо PIECE = nil, то знаходить всі фігури даного кольору. Вертає список полів, на яких стоять фігури.")
  (:method ((board board) piece-kind color)
    (loop :for sq :from 1 :to 64
       :when (and (not (null (aref (pieces board) sq)))
                  (or (not piece-kind)
                      (eql piece-kind (kind (aref (pieces board) sq))))
            (eql color (color (aref (pieces board) sq))))
       :collecting sq)))


(defgeneric value-of (object &key)
  (:documentation "Обраховує оцінку заданого обєкта."))


(defmethod value-of (p &key)
  "Метод, обробляє типи, а не класи."
  (cond
    ((typep p 'piece-kind)
     (let ((pv '(:pawn 1 :knight 3 :bishop 3 :rook 5 :queen 9 :king 100)))
       (getf pv p 0)))
    (t (error "Unsupported type in value-of"))))

(defmethod value-of ((the-piece piece) &key)
  (let ((val (value-of (kind the-piece))))
    ;(* val (if (eql (color the-piece) :white) +1 -1))))
    val))

(defun piece-value (piece &key (color :white))
  "Оцінка вартості фігури з точки зору COLOR"
  (if piece
      (* (value-of piece)
         (if (eq (color piece) color) +1 -1))
      0))

(defun piece-of-color (piece color)
  (and piece
       (eq (color piece) color)))

;;
;; Розстановка фігур
;;

(defgeneric clear (board)
  (:documentation "Очистити все з дошки")
  (:method ((board board))
    (progn
      (setf (slot-value board 'enpassant) nil)
      (setf (slot-value board 'turn) :white)
      (setf (slot-value board 'castlings) '(#@c1@ #@g1@ #@c8@ #@g8@))
      (fill (slot-value board 'pieces) nil)
      t)))


(defgeneric setup-piece (board sq piece &key)
  (:documentation "Ставить на поле SQ фігуру PIECE.")
  (:method ((board board) sq piece &key (replace t))
    "Якщо поле зайняте SQ, то фігура заміняється новою."
    (declare (ignorable replace))
    (setf (aref (slot-value board 'pieces) sq) piece)))


(defun piece-by-name (char)
  (cdr (assoc (char-upcase char)
	      '((#\K . :king) (#\Q . :queen) (#\R . :rook)
		(#\N . :knight) (#\B . :bishop) (#\P . :pawn)))))


(defmethod load-from-fen-string ((board board) fen-str)
  ;; Нотація FEN - рядок, що складається з:
  ;; - опис положення фігур (від 8-ой горизонталі до 1-ой, від a до h), ВЕЛИКІ
  ;;   букви позначають білі фігури, малі - чорні. Роздільник рядків - '/'.
  ;;   Цифри в рядку позначають кількість пустих клітин;
  ;; - хто ходить (b чи w);
  ;; - допустимі рокіровки (KQkq);
  ;; - поле взяття на проході в стандартній нотації чи '-';
  ;; - число півходів з моменту останнього ходу пішака або взяття фігури;
  ;; - число ходів, які зробили чорні (мінімум - 1).
  ;; Приклад : rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2
  (flet ((piece-by-name (char)
	   (cdr (assoc (char-upcase char)
		       '((#\K . :king) (#\Q . :queen) (#\R . :rook)
			 (#\N . :knight) (#\B . :bishop) (#\P . :pawn)))))
	 (piece-color (char)
	   (if (upper-case-p char)
	       :white
	       :black)))
    (let ((tokens (cl-utilities:split-sequence #\Space fen-str))
	  tok (sq 1) empty-count char)
      (clear board)
      (setf tok (pop tokens)) ; положення фігур на дошці
      (loop :for row :in (reverse (cl-utilities:split-sequence #\/ tok)) :do
	 (dotimes (i (length row))
	   (setf char (elt row i))
	   (setf empty-count (digit-char-p char))
	   (when (null empty-count)
	     (setf empty-count 1)
	     (setup-piece board sq
			  (make-instance 'piece :kind (piece-by-name char) :color (piece-color char))))
	   (incf sq empty-count)))
      (setf tok (pop tokens)) ; чий хід
      (setf (turn board) (if (char= (elt tok 0) #\w) :white :black))
      (setf tok (pop tokens)) ; рокіровка
      (setf (castlings board)
	    (mapcar #'(lambda (c) (cdr (assoc c '((#\K . #@g1@) (#\Q . #@c1@)
						  (#\k . #@g8@) (#\q . #@c8@)))))
		    (coerce tok 'list)))
      (setf tok (pop tokens)) ; взяття на проході
      (setf (enpassant board) (if (char= (elt tok 0) #\-) nil (string-to-square tok)))
      (setf tok (pop tokens)) ; число півходів
      (setf tok (pop tokens)) ; число ходів
      t)))


(defgeneric new-game (board)
  (:documentation "Початкова позиція.")
  (:method ((board board))
    (load-from-fen-string board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")))



;;; Рух фігур

(defun move-piece (board from to)
  (setf (aref (pieces board) to) (aref (pieces board) from))
  (setf (aref (pieces board) from) nil))


(defgeneric make-move (board move &key)
  (:documentation "Зробити хід на дошці.")
  (:method ((the-board board) move &key (clone-board nil))
    "Якщо clone-board != nil, то BOARD нне змінюється і хід повертає копію BOARD зі зробленим на ній ходом."
    (let* ((board (if clone-board (copy-board the-board) the-board))
	   (from (move-from move))
	   (to (move-to move))
	   (piece (whos-at board from))
	   (color (color piece)))
      ;; обробка рокіровок; переставити туру і заборонити рокіровки
      (let* ((e1e8 (if (eql color :white) #@e1@ #@e8@))
	     (c1c8 (- e1e8 2))
	     (g1g8 (+ e1e8 2))
	     (rook-sq-from (if (< 0 (- to from)) (+ to 1) (- to 2)))
	     (rook-sq-to (- to (signum (- to from)))))
	(cond
	  ((= from e1e8)
	   (setf (castlings board)
		 (remove-if #'(lambda (x) (member x '(c1c8 g1g8))) (castlings board)))
	   (when (castling-move-p piece color move)
	     (move-piece board rook-sq-from rook-sq-to))) ; переносимо туру
	  ((= from (- e1e8 4)) ; a1/8
	   (setf (castlings board) (remove c1c8 (castlings board))))
	  ((= from (+ 3 e1e8)) ; h1/8
	   (setf (castlings board) (remove g1g8 (castlings board))))))
      (setf (slot-value board 'turn) (opposite-color (slot-value board 'turn)))
      (move-piece board from to)
;      (when (not (null (move-transform move)))
;	(setf (kind piece) (move-transform move)))
      board)))


(defmethod initialize-instance :after ((the-board board) &key)
  (new-game the-board))


;;; макрос
(defmacro do-pieces ((board bind-vars &key (color nil)) &body body)
  "Виконує BODY для всіх значень SQ, які відповідають полям на яких стоять фігури кольору COLOR."
  (let (piece-bind (sq-bind (gensym)))
    (when (and (listp bind-vars) (= (length bind-vars) 2))
      (setf piece-bind (first bind-vars))
      (setf sq-bind (second bind-vars)))
    (when (symbolp bind-vars)
      (setf piece-bind bind-vars))
  `(do ((x-sq 1 (1+ x-sq)))
       ((> x-sq 64))
     (when (and (not (null (aref (pieces ,board) x-sq)))
		(or (null ,color) (eql ,color (color (aref (pieces ,board) x-sq)))))
       (let ((,piece-bind (aref (pieces ,board) x-sq))
	     (,sq-bind x-sq))
	 (declare (ignorable ,piece-bind) (ignorable ,sq-bind))
	 ,@body)))))

(defmacro with-move ((board from to) &body body)
  "Обраховує BODY в PROGN з ходом FROM-TO made на BOARD. Відміняє зміни викликані ходом після обрахунку."
  (let ((piece-at-to (gensym)))
  `(let ((,piece-at-to (whos-at ,board ,to)))
     (move-piece ,board ,from ,to)
     (prog1
         (progn ,@body)
       (move-piece ,board ,to ,from)
       (setup-piece ,board ,to ,piece-at-to)))))
