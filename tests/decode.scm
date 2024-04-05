#!/usr/bin/guile -s
!#
;; RLP decoding tests.
(import (only (rlp) rlp-decode)
	(srfi 64))

(load "data.scm")

(test-begin "bv-decode-test")
(for-each
 (lambda (datum)
   (test-equal (rlp-decode (cadr datum)) (car datum)))
 bv-test-data)
(test-end "bv-decode-test")

(test-begin "list-decode-test")
(for-each
 (lambda (datum)
   (test-equal (rlp-decode (cadr datum)) (car datum)))
 list-test-data)
(test-end "list-decode-test")
