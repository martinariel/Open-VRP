(in-package :open-vrp.test)

;; Break utilities
;; --------------------

(define-test closest-between-test
  "Test closest-between util"
  (:tag :breaks)
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

(define-test closest-between-from-list
  "Test closest-between-from-list util"
  (:tag :breaks)
  (let ((matrix (alist-to-hash '((:a          (:b . 1) (:c . 2) (:d . 7))
                                 (:b (:a . 3)          (:c . 5) (:d . 6))
                                 (:c (:a . 8) (:b . 2.5)          (:d . 3.5))
                                 (:d (:a . 3) (:b . 5) (:c . 1))))))
    (assert-equal :c (closest-between :a :b matrix '(:c)))
    (assert-equal :d (closest-between :a :c matrix '(:d)))
    (assert-equal :b (closest-between :a :d matrix '(:b)))
    (assert-equal :c (closest-between :b :a matrix '(:a :b :c)))))
