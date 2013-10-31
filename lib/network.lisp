;;; -----------------------------------------
;;; Distance matrix data-structure: Hash-table of hash-tables
;;; -----------------------------------------

(in-package :open-vrp.util)
(declaim (optimize speed))

(defun sethash (key val hash-table)
  "Setter for hash-table"
  (check-type hash-table hash-table)
  (setf (gethash key hash-table) val))

(defun alist-to-hash (alist)
  "Given an alist matrix, convert it into a hash table"
  (let ((matrix (make-hash-table)))
    (dolist (row alist)
      (sethash (first row) (make-hash-table) matrix)
      (dolist (col (rest row))
        (sethash (car col) (cdr col) (gethash (first row) matrix))))
    matrix))

(defun distance (from to dist-matrix)
  "Read from the distance-matrix with two keys (location IDs). Expects dist-matrix to be a hash table of hash tables."
  (check-type from symbol)
  (check-type to symbol)
  (check-type dist-matrix hash-table)
  (if (eq from to) 0
      (let ((row (gethash from dist-matrix)))
        (unless row (error 'distance-between-nodes-undefined :from from :to to))
        (check-type row hash-table)
        (aif (gethash to row)
             it
             (error 'distance-between-nodes-undefined :from from :to to)))))

;; -------------------------

;; Accessor functions
;;--------------------------

(defmethod node ((prob problem) id)
  (gethash id (problem-network prob)))

(defmethod visit-node ((prob problem) id)
  (gethash id (problem-visits prob)))

;; -------------------------
