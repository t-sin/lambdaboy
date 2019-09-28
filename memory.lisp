(defpackage #:lambdaboy.memory
  (:use #:cl)
  (:export))
(in-package #:lambdaboy.memory)

(defclass memory ())

(defgeneric mem-set (m addr))
(defgeneric mem-get (m addr v))

(defgeneric mem-setw (m addr))
(defgeneric mem-getw (m addr w))

(defclass memory-map (memory)
  map)
