;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Основные типы данных.
;;;;

(in-package :ccs)

(deftype color () '(member :white :black))
(deftype piece-kind () '(member :king :queen :rook :bishop :knight :pawn))
(deftype square ()
  "Поля задаються цілими числами з інтервалу [1,64]. Для перетворення рядків виду 'a3'  до типа square функція string-to-square або форма #@a3@."
  '(integer 1 64))

(defstruct (move
	     (:constructor create-move (from to &optional transform))
	     (:print-function print-move))
  "Структура для опису хода. BOA конструктор create-move. FROM и TO являются обовязкові поля, TRANSFORM тільки для ходу пішака на 8 горизонталь."
  (from nil :type square)
  (to nil :type square)
  (transform nil :type (or null piece-kind)))


(defmethod make-load-form ((m move) &optional env)
  (declare (ignore env))
  (list 'create-move (move-from m) (move-to m) (move-transform m)))
