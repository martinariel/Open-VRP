(in-package :open-vrp.test)

;; Break specific TS tools
;; --------------------
(define-test assess-insertion-moves-with-tw-and-breaks
  (:tag :break-algo)
  "Test assess-move while checking tw and updating breaks"
  (let* ((o1 (make-order :duration 1 :start 0 :end 11 :node-id :o1))
         (o2 (make-order :duration 2 :start 0 :end 20 :node-id :o2))
         (o3 (make-order :duration 3 :start 10 :end 13 :node-id :o3))
         (o4 (make-order :duration 4 :start 10 :end 14 :node-id :o4))
         (o5 (make-order :duration 5 :start 10 :end 15 :node-id :o5))
         (b1 (make-pitstop :duration 5 :start 10 :end 15 :node-id :b1 :break-type "long" :break-location :o5))
         (b2 (make-pitstop :duration 5 :start 10 :end 15 :node-id :b2 :break-type "long" :break-location :o5))
         (t1 (make-vehicle :id :t1 :route (list b1) :start-location :A :end-location :B :shift-end 25))
         (t2 (make-vehicle :id :t2 :route (list b2 o4) :start-location :A :end-location :B :shift-start 10 :shift-end 25))
         (dist {:o1 {      :o2 1 :o3 2 :o4 3 :o5 5 :A 1 :B 4}
                :o2 {:o1 1       :o3 1 :o4 2 :o5 4 :A 2 :B 3}
                :o3 {:o1 2 :o2 1       :o4 1 :o5 3 :A 3 :B 2}
                :o4 {:o1 3 :o2 2 :o3 1       :o5 1 :A 4 :B 1}
                :o5 {:o1 4 :o2 3 :o3 2 :o4 1       :A 6 :B 2}
                :A  {:o1 1 :o2 2 :o3 3 :o4 4 :o5 6      :B 5}
                :B  {:o1 4 :o2 3 :o3 2 :o4 1 :o5 2 :A 5     }})
         (bvrptw (make-instance 'bvrptw :fleet (list t1 t2)
                                :break-locations '(:o5 :o3)
                                :dist-matrix dist
                                :visits {:o1 o1 :o2 o2 :o3 o3 :o4 o4 :o5 o5})))
    (assert-equal -3 (assess-move bvrptw (make-insertion-move :node-id :o1 :vehicle-id :t1 :index 0)))
    (assert-equal 1 (assess-move bvrptw (make-insertion-move :node-id :o1 :vehicle-id :t1 :index 1)))
    (assert-equal -3 (assess-move bvrptw (make-insertion-move :node-id :o2 :vehicle-id :t1 :index 0)))
    (assert-equal -1 (assess-move bvrptw (make-insertion-move :node-id :o2 :vehicle-id :t1 :index 1)))))
