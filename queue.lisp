;;;; Simple FIFO for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :queue
  (:use :cl)
  (:export
   #:make-queue

   ;; push/pop elements on/off the queue
   #:queue-push
   #:queue-pop
   #:queue-list
   #:queue-peek
   #:queue-clear
   #:queue-size
   #:queue-empty-p))

(in-package :queue)

;;; ----------------------------------------------------

(defstruct (queue (:constructor %make-queue (xs &aux (ys (last xs)))))
  "Implements a list-based queue, capable of efficient insertions at the
   tail."
  (head xs :read-only t)
  (tail ys :read-only t))

;;; ----------------------------------------------------

(defun make-queue (&key initial-contents)
  "Create a new queue, optionally prepopulated with the elements of a
   specified list."
  (%make-queue (cons nil initial-contents)))

;;; ----------------------------------------------------

(defmethod print-object ((q queue) stream)
  "Output the queue to a stream."
  (print-unreadable-object (q stream :type t)
    (format stream "~:[EMPTY~;~:*~a~]" (rest (queue-head q)))))

;;; ----------------------------------------------------

(defun queue-push (x q)
  "Push a value onto the tail of the queue and return the inserted
   object."
  (with-slots (tail)
      q
    (car (setf tail (cdr (rplacd tail (list x)))))))

;;; ----------------------------------------------------

(defun queue-pop (q)
  "Pop a value off the queue, return the value and a success flag."
  (with-slots (head)
      q
    (when (rest head)
      (values (car (setf head (cdr head))) t))))

;;; ----------------------------------------------------

(defun queue-list (q)
  "Return the list of elements in the queue; does not alter the queue."
  (rest (queue-head q)))

;;; ----------------------------------------------------

(defun queue-peek (q)
  "Return without removing the first element of the queue in conjunction
   with a success flag."
  (when (rest (queue-head q))
    (values (car (rest (queue-head q))) t)))

;;; ----------------------------------------------------

(defun queue-clear (q)
  "Remove all elements of the queue and return the modified queue."
  (with-slots (head tail)
      q
    (setf head (cons nil nil))
    (setf tail (last head)))
  q)

;;; ----------------------------------------------------

(defun queue-size (q)
  "Return the number of elements in the queue."
  (length (rest (queue-head q))))

;;; ----------------------------------------------------

(defun queue-empty-p (q)
  "Check whether the queue is empty."
  (null (rest (queue-head q))))
