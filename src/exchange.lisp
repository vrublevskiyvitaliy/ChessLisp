;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Розмін на поле
;;;;

(in-package :ccs)


(defun exchange-value (board sq color)
  (let ((p (whos-at board sq)))
    (if (and p (eql color (color p)))
        (%exchange-value-aux board sq (opposite-color color))
        (%exchange-value-aux board sq color))))


(defun optimal-cut (trace color &optional (current (cons 0.0d0 nil)))
  "COLOR роблять перший хід в послідовності TRACE, зважена з точки білих."
  (if (null trace)
      current
      (let
          ((candidate (optimal-cut (cdr trace) (opposite-color color) (car trace))))
       ; (if (funcall (if (eql color :white) #'max #'min)
        (if (funcall (if (eql color :white) #'> #'<=)
                     (car current)
                     (car candidate))
            current
            candidate))))


(defun %exchange-value-aux (board sq color)
  "Обраховується значення розміну на поле SQ, при умові, що перший хід роблять COLOR."
  (let ((val 0.0d0)
        (t-board (copy-board board))
        exchange-trace
        piece-trace)
    (setf (turn t-board) color)
    (loop :for who = color :then (opposite-color who)
       :for direct-attackers = (attackers t-board sq :color who)
       :and p = (whos-at t-board sq)
       :and sign = (if (eql who :white) +1 -1)
       :for attack-piece-sq = (first (sort direct-attackers #'<
                               :key #'(lambda (x) (value-of (whos-at t-board x)))))
      :while (not (or (null direct-attackers) (null p)))
       :do (when (eql (color p) (opposite-color who))
             (push (cons (whos-at t-board attack-piece-sq)
                         attack-piece-sq)
                   piece-trace)
             (push (cons (incf val (* sign (value-of p)))
                         (copy-seq piece-trace))
                   exchange-trace))

       (make-move t-board
                  (create-move
                   attack-piece-sq
                   sq)))

    ;(print piece-trace)
    (destructuring-bind (ex-value . ex-pieces)
        (optimal-cut (nreverse exchange-trace) color)
      ;(print (list ex-value ex-pieces))
      (values ex-value t-board ex-pieces))))

(defun exchange-positive-p (value color &key strictly)
  (let ((cmp (if strictly #'< #'<=)))
    (if (eq color :white)
        (funcall cmp 0 value)
        (funcall cmp value 0))))

(defun better-exchange-p (value-new value-old color &key (strictly t))
  "Verify that exchange value VALUE-NEW is better than VALUE-OLD for COLOR."
  (let ((cmp (if strictly #'< #'<=)))
    (if (eq color :white)
        (funcall cmp value-old value-new)
        (funcall cmp value-new value-old))))
