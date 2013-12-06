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
              (t (call-next-method)))))))
