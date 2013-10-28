# Open-VRP lite

This is a rearchitectured version of Open-VRP, as small as can be, without the unnecessary tools. There is only Tabu Search, there are no init-macros nor drawers.

## Synopsis

Open VRP is a framework to model and solve [VRP-like](http://neo.lcc.uma.es/radi-aeb/WebVRP/) problems for students, academics, businesses and hobbyist alike. This framework allows for quick implementation of simple TSP/VRP problems to more complicated VRPTW, PDPTW, MDCPVRPPDTW, or however cool you want to sound. The library is extensibly written in Common Lisp's CLOS. Depending on your interest/purpose, an algorithm can be:

* [written from scratch](https://github.com/mck-/Open-VRP/wiki/Using-Open-VRP:-writing-your-algo-from-scratch)
* tweaked from existing implementations - (currently only [Tabu Search implemented](https://github.com/mck-/Open-VRP/wiki/Description-of-the-Tabu-Search-implementation))

The Problem object (e.g. VRP) and the Algorithm object (e.g. Genetic Algorithm) are modelled seperately and combined with the generic method (solve-prob problem algo). Different solution algorithms can be tested and compared against each other on the same problem (which you only model once).

## Current features (v. 0.9.0)

* TSP, VRP, CVRP, VRPTW, CVRPTW
* Homogenous/heterogenous fleet
* Demands, duration, capacity, time-windows, speed
* Define network using (asymettric) distance matrix
* Tabu Search
* Logging of search progress (to file or to REPL)
* Shift constraints for fleet

## Vision

Too often have I found myself having to build a VRP model from scratch, just to experiment with some meta-heuristics for a school paper. Academics/students with a background/interest in Mathematics/Operations Research without the skills/patience for die-hard coding (in C++/Java), have no choice but to spend their valuable time stuck in the debug/test/debug cycle. [Here](https://kuomarc.wordpress.com/2012/01/27/why-i-love-common-lisp-and-hate-java/) is [why](http://kuomarc.wordpress.com/2012/03/05/the-uncommon-lisp-approach-to-operations-research/) those in OR should consider Common Lisp as an option.

With this framework, I hope to catalyze the research and application of routing solutions. Researchers in innovative new algorithms should not need to fiddle in the Eclipse debugger screen. They should be able to focus all their energy and effort in devising their heuristics. OR should be kept fun and engaging.

The ultimate vision for Open VRP is a simple intuitive toolkit for the OR community, free for anyone.

## Installation

```
~$ git clone -b rearchitecture git://github.com/mck-/Open-VRP.git
```
Add this path and evaluate require:

```
(push "/path/to/Open-VRP/" asdf:*central-registry*)
(require 'open-vrp)
(in-package :open-vrp)
```

## License

Open-VRP is licensed under the terms of the [Lisp Lesser GNU
Public License](http://opensource.franz.com/preamble.html), known as
the LLGPL.  The LLGPL consists of a preamble (see above URL) and the
LGPL.  Where these conflict, the preamble takes precedence.
Open-VRP is referenced in the preamble as the "LIBRARY."
