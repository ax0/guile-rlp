#!/usr/bin/guile -s
!#
;; RLP encoding tests.
(import (only (rlp) rlp-encode)
	(srfi 64))

(load "data.scm")

(test-begin "bv-encode-test")
(for-each
 (lambda (datum)
   (test-equal (rlp-encode (car datum)) (cadr datum)))
 bv-test-data)
(test-end "bv-encode-test")

(test-begin "list-encode-test")
(for-each
 (lambda (datum)
   (test-equal (rlp-encode (car datum)) (cadr datum)))
 list-test-data)
(test-end "list-encode-test")
