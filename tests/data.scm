;; RLP encoding/decoding test data
;; Examples taken from https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/
(import
 (only (rlp) number->be-bytes rlp-encode)
 (scheme base)
 (srfi 1))
(define bv-test-data
  (zip
   (list ; Unencoded strings
    (string->utf8 "dog")
    (string->utf8 "")
    (number->be-bytes 0)
    (bytevector 0)
    (number->be-bytes 15)
    (number->be-bytes 1024)
    (string->utf8 "Lorem ipsum dolor sit amet, consectetur adipisicing elit"))
   (list ; Encoded strings
    (bytevector-append (bytevector #x83)
		       (string->utf8 "dog"))
    (bytevector #x80)
    (bytevector #x80)
    (bytevector #x00)
    (bytevector #x0f)
    (bytevector #x82 #x04 #x00)
    (bytevector-append (bytevector #xb8 #x38)
		       (string->utf8
			"Lorem ipsum dolor sit amet, consectetur adipisicing elit")))))
(define list-test-data
  (zip
   (list ; Unencoded lists
    (map string->utf8 '("cat" "dog"))
    (list)
    (list (list) (list (list)) (list (list) (list (list)))))
   (list ; Encoded lists
    (bytevector-append
     (bytevector #xc8 #x83)
     (string->utf8 "cat")
     (bytevector #x83)
     (string->utf8 "dog"))
    (bytevector #xc0)
    (bytevector #xc7 #xc0 #xc1 #xc0 #xc3 #xc0 #xc1 #xc0))))
