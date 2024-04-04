;; A library for RLP encoding and decoding.
;; RLP-encodable values are bytevectors and lists of RLP-encodable values.
(define-library (rlp)
  (export
   number->be-bytes
   be-bytes->number
   rlp-decode
   rlp-encode)
  (import
   (scheme base)
   (srfi 1)
   (only (srfi 4) u8vector->list list->u8vector))
  (begin

    ;; Useful procedures for converting between numbers and bytevectors.
    ;; 'be' indicates the big-endian convention.
    (define (number->be-bytelist n)
      ;; (if (zero? n)
      ;; 	  (list 0)
	  (let loop ((lis '())
		     (n n))
	    (if (zero? n) lis
		(loop (cons (modulo n 256) lis)
		      (quotient n 256)))))

    (define (number->be-bytes n)
      (list->u8vector (number->be-bytelist n)))

    (define (be-bytes->number bytes)
      (fold (lambda (b n) (+ b (* 256 n))) 0 (u8vector->list bytes)))

    ;;;;;;;;;;;;;;;;;;
    ;; RLP encoding ;;
    ;;;;;;;;;;;;;;;;;;

    ;; General-purpose RLP encoder dispatching on type.
    (define (rlp-encode obj)
      (cond ((bytevector? obj)
	     (bytevector->rlp-bytevector obj))
	    ((list? obj)
	     (list->rlp-bytevector obj))
	    (else #f)))

    
    ;; RLP encoder for bytevectors.
    (define (bytevector->rlp-bytevector bv)
      (define bv-length (bytevector-length bv))
      (cond ((and (= bv-length 1)
		  (< (bytevector-u8-ref bv 0) 128))
	     bv)
	    ((< bv-length 56)
	     (bytevector-append (bytevector (+ #x80 bv-length))
				bv))
	    (else
	     (let* ((bv-length-be-bytes (number->be-bytes bv-length))
		    (bv-length-bytelength (bytevector-length bv-length-be-bytes)))
	       (and (<= bv-length-bytelength 8) ; Out of bounds otherwise.
		    (bytevector-append
		     (bytevector (+ #xb7 bv-length-bytelength))
		     bv-length-be-bytes
		     bv))))))

    ;; RLP encoder for lists.
    (define (list->rlp-bytevector lis)
      (define rlp-lis (map rlp-encode lis))
      (define rlp-lis-length (fold + 0 (map bytevector-length rlp-lis)))
      (if (< rlp-lis-length 56)
	  (apply bytevector-append
		 (cons (bytevector (+ #xc0 rlp-lis-length))
		       rlp-lis))
	  (let* ((rlp-lis-length-be-bytes (number->be-bytes rlp-lis-length))
		 (rlp-lis-length-bytelength (bytevector-length rlp-lis-length-be-bytes)))
	    (and (<= rlp-lis-length-bytelength 8)
		 (apply bytevector-append
			(cons
			 (bytevector (+ #xf7 rlp-lis-length-bytelength))
			 (cons
			  rlp-lis-length-be-bytes
			  rlp-lis)))))))

    ;;;;;;;;;;;;;;;;;;
    ;; RLP decoding ;;
    ;;;;;;;;;;;;;;;;;;
    
    (define (rlp-string-prefix? prefix)
      (< prefix #xc0))

    (define (rlp-list-prefix? prefix)
      (not (rlp-string-prefix? prefix)))

    (define (rlp-decode-with-ptr rlp-bv ptr)
      (if (rlp-string-prefix? (bytevector-u8-ref rlp-bv ptr))
	  (rlp-string->bytevector-with-ptr rlp-bv ptr)
	  (rlp-list->bytevector-with-ptr rlp-bv ptr)))

    ;; RLP length decoder. Returns payload length and pointer
    ;; to payload.
    (define (rlp-decode-length rlp-bv ptr)
      (define prefix (bytevector-u8-ref rlp-bv ptr))
      (if (rlp-string-prefix? prefix)
	  (cond ((< prefix #x80) (values 1 ptr))
		((< prefix #xb8) (values (- prefix #x80) (+ ptr 1)))
		(else
		 (let* ((payload-length-bytelength (- prefix #xb7))
			(payload-length (be-bytes->number (bytevector-copy rlp-bv (+ ptr 1) (+ ptr 1 payload-length-bytelength))))
			(payload-offset (+ ptr payload-length-bytelength 1)))
		   (values payload-length payload-offset))))
	  (if (< prefix #xf8)
	      (values (- prefix #xc0) (+ ptr 1))
	      (let* ((payload-length-bytelength (- prefix #xf7))
		     (payload-length (be-bytes->number (bytevector-copy rlp-bv (+ ptr 1) (+ ptr 1 payload-length-bytelength))))
		     (payload-offset (+ ptr payload-length-bytelength 1)))
		(values payload-length payload-offset)))))

    ;; Procedure for decoding an RLP-encoded string.
    ;; Returns a bytevector and a pointer to the byte following
    ;; the end of the RLP payload.
    (define (rlp-string->bytevector-with-ptr rlp-str ptr)
      (define prefix (bytevector-u8-ref rlp-str ptr))
      (define-values (length offset) (rlp-decode-length rlp-str ptr))
      (values
       (bytevector-copy rlp-str offset (+ offset length))
       (+ offset length)))

    ;; Procedure for decoding an RLP-encoded list.
    ;; Returns a list and a pointer to the byte following
    ;; the end of the RLP payload.
    (define (rlp-list->bytevector-with-ptr rlp-lis ptr)
      (define-values (length offset) (rlp-decode-length rlp-lis ptr))
      (define end-ptr (+ offset length))
      (let loop ((ptr offset)
		 (lis '()))
	(cond ((= ptr end-ptr)
	       (values (reverse lis) end-ptr))
	      ((< ptr end-ptr)
	       (let-values (((decoded-obj new-ptr) (rlp-decode-with-ptr rlp-lis ptr)))
		 (loop new-ptr (cons decoded-obj lis))))
	      (else (error "Invalid RLP encoding")))))

    (define (rlp-decode rlp-bv)
      (let-values (((decoded-obj -end-ptr) (rlp-decode-with-ptr rlp-bv 0)))
	decoded-obj))))
