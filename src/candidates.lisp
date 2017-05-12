;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Вибір ходів кандидатів
;;;;

(in-package :ccs)

(labels
    ((find-valid-moves (board color &aux res)
       "Повертає список допустимих ходів фігурами кольору color."
       (do-pieces (board (p from) :color color)
         (push
          (mapcan #'(lambda (to &aux m)
                      (setf m (create-move from to))
                      (if (valid-move-p board m :valid-turn nil) (list m) nil))
                  (moves p from))
          res))
       (apply #'append res)))

  (defun candidate-moves (board &key (color (turn board)))
    "Пошук кращих ходів. Повертає список cons-ів хід.оцінка. Оцінка зі сторони білих."
    (let ((possible-moves (find-valid-moves board color))
	  moves-values
	  (better-move-p (if (eql color :white) #'>= #'<=)))  ; Порівняння в залежності від кольору
      ;; оцінимо ходи
      (setf moves-values
	    (sort
	     (mapcar #'(lambda (m)
                         (log-message :trace "Evaluating move ~A-~A"
                                      (square-to-string (move-from m))
                                      (square-to-string (move-to m)))
			 (cons m (value-of (make-move board m :clone-board t))))
		     possible-moves)
             better-move-p
             :key #'cdr))
      moves-values))
)
