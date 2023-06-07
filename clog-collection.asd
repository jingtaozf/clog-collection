;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;;
;; Filename: clog-collection.asd
;; Description: A set of CLOG Plugins
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2023.05.31 21:27:16(+0800)
;; Last-Updated: 2023.06.07 21:51:31(+0800)
;;     Update #: 21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clog-collection.system)
    (defpackage :clog-collection.system
      (:use :cl :asdf))))

(in-package :clog-collection.system)

(asdf:defsystem clog-collection
  :author "Xu Jingtao <jingtaozf@gmail.com>"
  :version "0.1"
  :licence "Private"
  :serial t
  :description "A set of CLOG Plugins"
  :defsystem-depends-on (:literate-lisp)
  :depends-on (#:iterate #:alexandria #:clog #:yason)
  :entry-point
  "clog-collection:start-test"
  :components ((:module :base :pathname "./lisp"
                :serial t
                :components ((:org "cytoscape")))))
