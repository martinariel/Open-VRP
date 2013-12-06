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

(defun closest-between (from to dist-matrix &optional allowed-nodes)
  "Given two location IDs, return a third location X (with best-distance of detour) which results in the shortest distance from->X + X->to -- to be used to find the best break locations. Only consider X from allowed-nodes subset if provided"
  (labels ((iter (node best-dist rest)
             (let ((x (car rest)))
               (cond ((null rest)
                      (unless node (error "No break locations!"))
                      (values node best-dist))
                     ((or (eq x from)
                          (eq x to)
                          (and allowed-nodes (not (member x allowed-nodes)))) ; x not in allowed-nodes
                      (iter node best-dist (cdr rest))) ; skip x
                     (t
                      (let ((dist (+ (distance from x dist-matrix)
                                     (distance x to dist-matrix))))
                        (if (or (null best-dist) (< dist best-dist))
                            (iter x dist (cdr rest))
                            (iter node best-dist (cdr rest)))))))))
    (iter nil nil (loop for node-id being the hash-keys of dist-matrix collect node-id))))

;; -------------------------

;; Accessor functions
;;--------------------------

(defmethod node ((prob problem) id)
  (gethash id (problem-network prob)))

(defmethod visit-node ((prob problem) id)
  (gethash id (problem-visits prob)))

;; -------------------------
