;;; All class definitions for CLOS VRP
;;; Node, Vehicle, Problem, Drawer and Algo objects
(in-package :open-vrp.classes)
(declaim (optimize speed))

;; Default constants
(defconstant +min-time+ 0)
(defconstant +max-time+ 1440)

;; The node object
;; ----------------------

(defstruct node
  "The super location class, defines a place with optionally x/y coords for plotting"
  (id (gensym) :type symbol :read-only t)
  (name "Nameless Node" :type string))

(defstruct visit
  "These are the actual places on the network that need to be visited, and can be depots, orders, or breaks -- they are linked to a location by node ID"
  (node-id (gensym) :type symbol :read-only t)
  (start +min-time+ :type fixnum :read-only t)
  (end +max-time+ :type fixnum :read-only t)
  (duration 0 :type fixnum :read-only t))

(defstruct (order (:include visit))
  "Order that needs to be visited."
  (demand 0 :type fixnum :read-only t))

(defstruct (pitstop (:include visit))
  "Location that represents a break/pitstop"
  (break-type "short" :type string)
  (break-location (gensym) :type symbol))

;; --------------------------

;; The vehicle object
;; ---------------------------
;; - route is a list of visit-ids

(defstruct vehicle
  (id (gensym) :type symbol :read-only t)
  (route nil :type list)
  (start-location :nil :type symbol :read-only t)
  (end-location :nil :type symbol :read-only t)
  (speed 1 :type number :read-only t)
  (shift-start +min-time+ :type fixnum :read-only t)
  (shift-end +max-time+ :type fixnum :read-only t)
  capacity
  (break-start 1100 :type fixnum :read-only t)
  (break-end 1400 :type fixnum :read-only t)
  (break-duration 100 :type fixnum :read-only t))


  ;; Todo:
  ;; type

;; ----------------------------

;; The problem object class
;; -----------------------
;; NOTE: The <Problem> object is also a <Solution> object interchangably.

;; Data structures:
;; - network is a Hash Table
;; - dist-matrix is a Hash Table of Hash Tables
;; - fleet is a list (see optimization notes)

(defclass problem ()
  ((name :reader problem-name :initarg :name :initform "VRP")
   (desc :reader problem-desc :initarg :desc :initform "Vehicle Routing Problem")
   (network :reader problem-network :initarg :network :type hash-table)
   (visits :reader problem-visits :initarg :visits :type hash-table)
   (dist-matrix :accessor problem-dist-matrix :initarg :dist-matrix :initform nil :type hash-table)
   (fleet :reader problem-fleet :initarg :fleet :type list)
   (allow-unserved :reader problem-allow-unserved :initarg :allow-unserved :initform T)
   (unserved :accessor problem-unserved :initarg :unserved :initform nil :type list)
   (log-file :accessor problem-log-file :initarg :log-file :initform nil)
   (log-mode :accessor problem-log-mode :initarg :log-mode :initform :repl)))
   ;; log-mode :none = off, :file = output file, :repl = REPL

(defclass CVRP (problem)
  ((name :initform "CVRP")
   (desc :initform "Capacitated Vehicle Routing Problem")))

(defclass VRPTW (problem)
  ((name :initform "VRPTW")
   (desc :initform "Vehicle Routing Problem with Time Windows")))

(defclass CVRPTW (CVRP VRPTW)
  ((name :initform "CVRPTW")
   (desc :initform "Capacitated Vehicle Routing Problem with Time Windows")))

(defclass BVRPTW (problem)
  ((name :initform "BVRPTW")
   (desc :initform "VRPTW with meal breaks and short breaks")
   (break-locations :initarg :break-locations :accessor problem-break-locations)))

;; ----------------------

;; Algo class
;; -----------------------

(defclass algo ()
  ((name :reader algo-name :initarg :name)
   (desc :reader algo-desc :initarg :desc)
   (best-sol :accessor algo-best-sol :initarg :best-sol :initform nil)
   (best-fitness :accessor algo-best-fitness :initarg :best-fitness :initform nil)
   (best-iteration :accessor algo-best-iteration :initform 0)
   (current-sol :accessor algo-current-sol :initarg :current-sol :initform nil)
   (iterations :accessor algo-iterations :initarg :iterations)))
