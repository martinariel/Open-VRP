;;; CLOS package descriptions
;;; -----------------------------

(defpackage :open-vrp.classes
  (:use :common-lisp)
  (:export :node
           :visit
           :order
           :pitstop
           :vehicle
           :problem
           :CVRP
           :VRPTW
           :CVRPTW
           :BVRPTW

           ;; algo
           :algo
           :name
           :desc
           :iterations

           ;; constructor functions
           :make-node
           :make-order
           :make-pitstop
           :make-vehicle

           ;; predicates
           :node-p
           :visit-p
           :order-p
           :pitstop-p
           :vehicle-p

           ;; accessor functions
           :node-id
           :node-name
           :visit-node-id
           :visit-start
           :visit-end
           :visit-duration
           :order-demand

           :pitstop-break-location
           :pitstop-break-type

           :vehicle-id
           :vehicle-route
           :vehicle-capacity
           :vehicle-speed
           :vehicle-start-location
           :vehicle-end-location
           :vehicle-shift-start
           :vehicle-shift-end
           :vehicle-break-start
           :vehicle-break-end
           :vehicle-break-duration

           :problem-name
           :problem-desc
           :problem-network
           :problem-visits
           :problem-dist-matrix
           :problem-fleet
           :problem-allow-unserved
           :problem-unserved
           :problem-log-file
           :problem-log-mode
           :problem-break-locations

           :algo-name
           :algo-desc
           :algo-best-sol
           :algo-best-fitness
           :algo-best-iteration
           :algo-current-sol
           :algo-iterations))

(defpackage :open-vrp.util
  (:use :common-lisp
        :open-vrp.classes
        :vecto)
  (:import-from	:alexandria :shuffle :flatten :with-gensyms :mean :standard-deviation)
  (:import-from :cl-fad :walk-directory)
  #+sbcl (:import-from :sb-mop :class-slots :slot-definition-name)
  #+(or allegro clisp lispworks) (:import-from :clos :class-slots :slot-definition-name)
  #+cmu (:import-from :mop :class-slots :slot-definition-name)
  (:export
   ;; simple utils
   :while
   :aif
   :it
   :copy-object

   ;; list utils
   :get-min-index
   :get-max-index
   :sort-ignore-nil
   :insert-before
   :insert-at-end
   :remove-index

   ;; route utils
   :no-visits-p
   :get-busy-vehicles
   :one-destination-p
   :insert-node
   :append-node
   :remove-node-id
   :add-to-unserved
   :remove-from-unserved

   ;; network utils
   :sethash
   :alist-to-hash
   :distance
   :closest-between
   :node
   :visit-node

   ;; fleet utils
   :route-indices
   :vehicle-with-node-id
   :node-on-route-p
   :route-dist
   :total-dist
   :vehicle
   :veh-route-times
   :route-times

   ;; time utils
   :time-to-minutes
   :minutes-to-time

   ;; constraint utils
   :constraints-check
   :constraints-p
   :in-capacity-p
   :travel-time
   :time-after-visit
   :veh-in-time-p
   :in-time-p

   :fitness
   :*unserved-penalty*

   ;; solver
   :init-algo
   :run-algo
   :*algo-backup*
   :solve-prob
   :multi-run
   :get-best-solution-from-multi-run
   :multi-run-algo
   :iterate
   :iterate-more
   :*start-time*
   :*multi-run-start-time*

   ;; output
   :print-routes
   :print-multi-run-stats
   :print-final-results
   :print-vrp-object
   :plot-solution
   :plot-nodes
   :print-timestamp
   :with-log-or-print
   :log-to-repl-p

   ;; conditions
   :same-origin-destination
   :distance-between-nodes-undefined
   :list-of-nils
   :index-out-of-bounds
   :unknown-log-mode
   :too-late-arrival
   :vehicle-not-found
   :no-feasible-solution))

(defpackage :open-vrp.algo
  (:use :common-lisp
        :open-vrp.classes
        :open-vrp.util)
  (:import-from :alexandria :shuffle :flatten)
  (:export
   ;; tools
   :fitness-before-after
   :feasible-move-p

   ;; best insertion
   :get-best-insertion-move-in-vehicle
   :get-best-insertion-move

   ;; conditions
   :no-initial-feasible-solution
   :no-feasible-move

   ;; iterator
   :initialize
   :generate-moves
   :perform-move
   :assess-move
   :assess-moves
   :sort-moves
   :select-move

   ;; move
   :move
   :move-node-ID
   :move-vehicle-ID
   :move-index
   :move-fitness
   :make-insertion-move
   :make-TS-best-insertion-move

   ;; algo-objects
   :greedy-best-insertion
   :tabu-list
   :tabu-search

   ;; Tabu Search object
   :ts-move-type
   :ts-init-heur
   :ts-aspiration-p
   :ts-elite-list-p
   :ts-tabu-list
   :ts-tenure
   :ts-parameter-f
   :ts-candidate-list
   :ts-stopping-condition

   ;; Tabu Search utils
   :add-to-tabu
   :add-move-to-tabu
   :clear-tabu-list
   :is-tabu-p
   :is-tabu-move-p
   :TS-best-insertion-move
   :all-moves-tabu))

(defpackage :open-vrp
  (:use :common-lisp
        :open-vrp.classes
        :open-vrp.util
        :open-vrp.algo)
  (:import-from	:alexandria :shuffle :flatten :with-gensyms)
  (:export :define-problem
           :load-testcase-Solomon
           :load-tsplib-vrp-file
           :multi-run-algo
           :solve-prob
           :iterate-more
           :plot-solution
           :print-routes

           ;; utils
           :route-times
           :route-indices
           :algo-best-sol

           ;; algos
           :tabu-search
           :greedy-NN
           :greedy-append
           :greedy-best-insertion))

           ;; demos
           ;; :test-tsp
           ;; :test-vrp
           ;; :solomon25
           ;; :solomon100
           ;; :christofides-1
           ;; :christofides-2))
