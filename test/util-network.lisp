(in-package :open-vrp.test)

;; Node/Network utilities
;; --------------------

(define-test distance-test
  "Test the distance util, which accesses the dist-matrix' hash of hash"
  (:tag :network)
  (let ((matrix (alist-to-hash '((:a (:b . 1) (:c . 2)) (:b (:a . 3) (:c . 5)) (:c (:a . 8) (:b . 2))))))
    (assert-equal 1 (distance :a :b matrix))
    (assert-equal 2 (distance :a :c matrix))
    (assert-equal 3 (distance :b :a matrix))
    (assert-equal 5 (distance :b :c matrix))
    (assert-equal 8 (distance :c :a matrix))
    (assert-equal 2 (distance :c :b matrix))
    (assert-error 'simple-type-error (distance "c" "a" matrix))
    (assert-error 'simple-type-error (distance :a :c '((:a (:c . 2)))))
    (assert-error 'distance-between-nodes-undefined (distance :d :c matrix))
    (assert-error 'distance-between-nodes-undefined (distance :c :d matrix))))

(define-test travel-time-test
  "Test the travel-time util, which is the same as dist-matrix if the speed is 1."
  (:tag :network)
  (let ((matrix (alist-to-hash '((:a (:b . 1) (:c . 2)) (:b (:a . 3) (:c . 5)) (:c (:a . 8) (:b . 2))))))
    (assert-equal 1 (travel-time :a :b matrix))
    (assert-equal 2 (travel-time :a :c matrix))
    (assert-equal 3 (travel-time :b :a matrix))
    (assert-equal 5 (travel-time :b :c matrix))
    (assert-equal 8 (travel-time :c :a matrix))
    (assert-equal 2 (travel-time :c :b matrix))
    (assert-error 'simple-type-error (travel-time "c" "a" matrix))
    (assert-error 'simple-type-error (travel-time :a :c '((:a (:c . 2)))))
    (assert-error 'distance-between-nodes-undefined (travel-time :d :c matrix))

    ;; With speed
    (assert-equalp 10 (travel-time :b :c matrix :speed 0.5))
    (assert-equal 4 (travel-time :c :a matrix :speed 2))
    (assert-equal 1/5 (travel-time :c :b matrix :speed 10))
    (assert-error 'simple-type-error (travel-time :b :c matrix :speed "2"))))


(define-test travel-time-test
  "Test closest-between util"
  (:tag :network)
  (let ((matrix (alist-to-hash '((:a          (:b . 1) (:c . 2) (:d . 7))
                                 (:b (:a . 3)          (:c . 5) (:d . 6))
                                 (:c (:a . 8) (:b . 2.5)          (:d . 3.5))
                                 (:d (:a . 3) (:b . 5) (:c . 1))))))
    (assert-equal :c (closest-between :a :b matrix))
    (assert-equal :b (closest-between :a :c matrix))
    (assert-equal :c (closest-between :a :d matrix))
    (assert-equal :d (closest-between :b :a matrix))
    (assert-equal :a (closest-between :b :c matrix))
    (assert-equal :c (closest-between :b :d matrix))
    (assert-equal :b (closest-between :c :a matrix))
    (assert-equal :d (closest-between :c :b matrix))
    (assert-equal :b (closest-between :c :d matrix))
    (assert-equal :b (closest-between :d :a matrix))
    (assert-equal :c (closest-between :d :b matrix))
    (assert-equal :a (closest-between :d :c matrix))))
