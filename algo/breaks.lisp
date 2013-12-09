(in-package :open-vrp.algo)

;;; Break specific algo tools
;;; ----------------
;;; 0. feasible-move-p
;;; 1. assess-move
;;; 2. perform-move
;;; 3. constraints checking

(defmethod feasible-move-p and ((sol BVRPTW) (m insertion-move))
  (with-slots (node-id vehicle-id index) m
    (let* ((v (vehicle sol vehicle-id))
           (route (vehicle-route v))
           (ins-node (visit-node sol node-id)))
      (labels ((get-location (node)
                 (if (pitstop-p node)
                     (pitstop-break-location node)
                     (visit-node-id node)))
               (iter (route time loc i)
                 (if (and (null route) (< i 0))
                     (values (<= (+ time (travel-time loc (vehicle-end-location v) (problem-dist-matrix sol) :speed (vehicle-speed v)))
                                 (vehicle-shift-end v))
                             time loc)
                     (let* ((to (if (= 0 i) ins-node (car route)))
                            (arr-time (+ time (travel-time loc (get-location to) (problem-dist-matrix sol)))))
                       (and
                        (<= arr-time (visit-end to))
                        (iter (if (= 0 i) route (cdr route))
                              (time-after-visit to arr-time)
                              (get-location to)
                              (1- i)))))))
        (iter route (vehicle-shift-start v) (vehicle-start-location v) index)))))

                       ;; (progn
                       ;;   (format t "Route: ~A~% Loc: ~A~% To: ~A~% Time: ~A~% Arr-time: ~A~% Node-start: ~A~% Node-end: ~A~% Duration: ~A~% ins-node-end: ~A~% i: ~A~%" (mapcar #'visit-node-id route) loc (visit-node-id to) time arr-time (visit-start to) (visit-end to) (visit-duration to) (visit-end ins-node) i)


(defmethod assess-move ((sol bvrptw) (m insertion-move))
  "BVRPTW specific assess-move for insertions, which updates the break location if it is being inserted next to it.

      Cases of inserting X:
      If normal, just calculate insertion cost of X

      If node-after is a pitstop:
        - subtract node-before -> current pitstop
        - subtract current pitstop -> node-after-pitstop
        - add node-before -> location X
        - update pitstop location with closest-between X and node-after-pitstop
        - ^ will return detour distance

      If node-before is a pitstop:
        - subtract node-before -> current pitstop
        - subtract current pitstop -> node-before-pitstop
        - add location X -> node-after
        - update pitstop location with closest-between node-before-pitstop and X
        - ^ will return detour distance"
  (with-slots (node-id vehicle-id index) m
    (let* ((veh (vehicle sol vehicle-id))
           (route (vehicle-route veh))
           (dist-matrix (problem-dist-matrix sol))
           (node-before (if (= index 0)
                            (vehicle-start-location veh)
                            (nth (1- index) route)))
           (node-after (if (= index (length route))
                           (vehicle-end-location veh)
                           (nth index route)))
           (node-before-id (if (symbolp node-before) node-before (visit-node-id node-before)))
           (node-after-id (if (symbolp node-after) node-after (visit-node-id node-after))))
      (cond ((pitstop-p node-after)
             (let* ((pit (pitstop-break-location node-after))
                    (node-after-pit (if (= (1+ index) (length route))
                                        (vehicle-end-location veh)
                                        (visit-node-id (nth (1+ index) route))))
                    (dist-to-pit (distance node-before-id pit dist-matrix)) ;subtract
                    (dist-from-pit (distance pit node-after-pit dist-matrix)) ;subtract
                    (dist-to-new (distance node-before-id node-id dist-matrix))) ;add
               (multiple-value-bind (loc dist-detour)
                   (closest-between node-id node-after-pit dist-matrix (problem-break-locations sol))
                 (declare (ignore loc))
                 (setf (move-fitness m)
                       (handler-case
                           (- (+ dist-detour dist-to-new)
                              (+ dist-to-pit dist-from-pit))
                         (distance-between-nodes-undefined () nil))))))
            ((pitstop-p node-before)
             (let* ((pit (pitstop-break-location node-before))
                    (node-before-pit (if (= index 1)
                                         (vehicle-start-location veh)
                                         (visit-node-id (nth (- index 2) route))))
                    (dist-to-pit (distance node-before-pit pit dist-matrix)) ;subtract
                    (dist-from-pit (distance pit node-after-id dist-matrix)) ;subtract
                    (dist-to-new (distance node-id node-after-id dist-matrix))) ;add
               ;; (format t "pit: ~A~% node-before-pit: ~A~% dist-to-pit: ~A~% dist-from-pit: ~A~% dist-to-new:~A" pit node-before-pit dist-to-pit dist-from-pit dist-to-new)
               (multiple-value-bind (loc dist-detour)
                   (closest-between node-before-pit node-id dist-matrix (problem-break-locations sol))
                 (declare (ignore loc))
                 ;; (format t "loc: ~A~% dist-detour: ~A~%" loc dist-detour)
                 ;; (print (- (+ dist-detour dist-to-new)
                 ;;              (+ dist-to-pit dist-from-pit)))
                 (setf (move-fitness m)
                       (handler-case
                           (- (+ dist-detour dist-to-new)
                              (+ dist-to-pit dist-from-pit))
                         (distance-between-nodes-undefined () nil))))))
            (t (setf (move-fitness m)
                     (handler-case
                         (-
                          (+ (distance node-before-id node-id dist-matrix)
                             (distance node-id node-after-id dist-matrix))
                          (distance node-before-id node-after-id dist-matrix))
                       (distance-between-nodes-undefined () nil))))))))

(defmethod perform-move ((sol bvrptw) (m insertion-move))
  "Checks if move is next to a break location, if so, update break location"
  (if (eq (type-of m) 'TS-best-insertion-move)
      (call-next-method)
      (with-slots (node-id vehicle-id index) m
        (let* ((veh (vehicle sol vehicle-id))
               (route (vehicle-route veh))
               (dist-matrix (problem-dist-matrix sol)))
          (let ((node-before (if (= index 0)
                                 (vehicle-start-location veh)
                                 (nth (1- index) route)))
                (node-after (if (= index (length route))
                                (vehicle-end-location veh)
                                (nth index route))))
            (cond ((pitstop-p node-after)
                   (let ((node-after-pit (if (= (1+ index) (length route))
                                             (vehicle-end-location veh)
                                             (visit-node-id (nth (1+ index) route)))))
                     (setf (pitstop-break-location node-after)
                           (closest-between node-id node-after-pit dist-matrix (problem-break-locations sol)))
                     (call-next-method)))
                  ((pitstop-p node-before)
                   (let ((node-before-pit (if (= index 1)
                                              (vehicle-start-location veh)
                                              (visit-node-id (nth (- index 2) route)))))
                     (setf (pitstop-break-location node-before)
                           (closest-between node-before-pit node-id dist-matrix (problem-break-locations sol)))
                     (call-next-method)))
                  (t (call-next-method))))))))

(defmethod assess-move ((sol bvrptw) (mv TS-best-insertion-move))
  (with-slots (node-id vehicle-id fitness) mv
    (flet ((insertion-cost (veh-id node-id)
             "Cost of inserting node-id into veh-id, calculated using best-insertion-move, unless veh-id is :UNSERVED"
             (if (eq :UNSERVED veh-id)
                 *unserved-penalty*
                 (move-fitness (get-best-insertion-move-in-vehicle sol veh-id node-id)))))
      (let ((v-w-n-id (vehicle-with-node-id sol node-id)))
        (handler-case
            ;; Calculate move differently when it involves the :UNSERVED list
            (if (eq :UNSERVED v-w-n-id)
                (setf fitness (- (insertion-cost vehicle-id node-id) *unserved-penalty*))
                (let* ((dist-matrix (problem-dist-matrix sol))
                       (veh (vehicle sol v-w-n-id))
                       (route (vehicle-route veh))
                       (pos (position node-id route :key #'visit-node-id))
                       (node-before (if (= pos 0) (vehicle-start-location veh) (nth (1- pos) route)))
                       (node-after (if (= (1+ pos) (length route)) (vehicle-end-location veh) (nth (1+ pos) route)))
                       (node-before-id (if (symbolp node-before) node-before (visit-node-id node-before)))
                       (node-after-id (if (symbolp node-after) node-after (visit-node-id node-after))))
                  (cond ((pitstop-p node-after)
                                        ; if node-after the one being removed is a pitstop, update that location by calculating:
                                        ;  [0 - 1 - B - 2 - 3]
                                        ; + calculate new B between 0 and 2
                                        ; - remove leg 1-B and B-2
                                        ; - remove leg 0-1
                         (let* ((pit (pitstop-break-location node-after))
                                (node-after-pit (if (= (+ 2 pos) (length route))
                                                    (vehicle-end-location veh)
                                                    (visit-node-id (nth (+ 2 pos) route))))
                                (dist-before (distance node-before-id node-id dist-matrix)) ;subtract
                                (dist-to-pit (distance node-id pit dist-matrix)) ;subtract
                                (dist-from-pit (distance pit node-after-pit dist-matrix))) ;subtract
                           ;; (format t "pit: ~A~% node-after-pit: ~A~% dist-to-pit: ~A~% dist-from-pit: ~A~% dist-before:~A~%" pit node-after-pit dist-to-pit dist-from-pit dist-before)
                           (multiple-value-bind (loc dist-detour)
                               (closest-between node-before-id node-after-pit dist-matrix (problem-break-locations sol))
                             (declare (ignore loc))
                             ;; (format t "loc: ~A~% dist-detour: ~A~%" loc dist-detour)
                             ;; (print (insertion-cost vehicle-id node-id))
                             (setf fitness
                                   (handler-case
                                       (+ (insertion-cost vehicle-id node-id)
                                        ;cost of removing:
                                          (- dist-detour ;   new B between 0 and 2
                                             dist-before ;   0-1
                                             dist-to-pit ;   1-B
                                             dist-from-pit)) ; B-2
                                     (distance-between-nodes-undefined () nil))))))
                        ((pitstop-p node-before)
                                        ; if node-before the one being removed is a pitstop, update that location by calculating:
                                        ;  [1 - B - 2 - 3]
                                        ; + calculate new B between 1 and 3
                                        ; - remove leg 1-B and B-2
                                        ; - remove leg 2-3
                         (let* ((pit (pitstop-break-location node-before))
                                (node-before-pit (if (= pos 1)
                                                     (vehicle-start-location veh)
                                                     (visit-node-id (nth (- pos 2) route))))
                                (dist-to-pit (distance node-before-pit pit dist-matrix)) ;subtract 1-B
                                (dist-from-pit (distance pit node-id dist-matrix)) ;subtract B-2
                                (dist-after (distance node-id node-after-id dist-matrix))) ;subtract 2-3
                           ;; (format t "pit: ~A~% node-before-pit: ~A~% dist-to-pit: ~A~% dist-from-pit: ~A~% dist-after:~A~%" pit node-before-pit dist-to-pit dist-from-pit dist-after)
                           (multiple-value-bind (loc dist-detour)
                               (closest-between node-before-pit node-after-id dist-matrix (problem-break-locations sol))
                             (declare (ignore loc))
                             ;; (format t "loc: ~A~% dist-detour: ~A~%" loc dist-detour)
                             ;; (print (insertion-cost vehicle-id node-id))
                             ;; (print (- (insertion-cost vehicle-id node-id)
                             ;;           (- dist-detour
                             ;;                 dist-to-pit
                             ;;                 dist-from-pit
                             ;;                 dist-after)))
                             (setf fitness
                                   (handler-case
                                       (+ (insertion-cost vehicle-id node-id)
                                        ;cost of removing:
                                          (- dist-detour
                                             dist-to-pit
                                             dist-from-pit
                                             dist-after))
                                     (distance-between-nodes-undefined () nil))))))
                        (t
                                        ; By default, just calculate savings:
                                        ; [1 - 2 - 3]
                                        ; subtract 1-2 and 2-3
                                        ; add 1-3
                         (setf fitness
                               (handler-case
                                   (- (insertion-cost vehicle-id node-id)
                                        ;save by removing:
                                      (let ((dist-before (handler-case (distance node-before-id node-id dist-matrix)
                                                           (same-origin-destination () 0))) ; 1-2
                                            (dist-after (handler-case (distance node-id node-after-id dist-matrix)
                                                          (same-origin-destination () 0))) ; 2-3
                                            (dist-between (handler-case (distance node-before-id node-after-id dist-matrix)
                                                            (same-origin-destination () 0)))) ; 1-3
                                        (- dist-between
                                           dist-after
                                           dist-before)))
                                 (distance-between-nodes-undefined () nil)))))))
          (no-feasible-move () (setf fitness nil)))))))
