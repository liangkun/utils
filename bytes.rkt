#!/usr/bin/env racket
#lang racket/base

;; Copyright (c) 2013 Liang Kun. All Rights Reserved.
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(require racket/cmdline)
(require rackunit)

;; command line arguments
(define skip (make-parameter 0))
(define encode? (make-parameter #t))
(define input (make-parameter #f))
(define test (make-parameter #f))

;; utility function to check invariables
(define (assert value message)
  (when (not value)
    (error message)))

;; constant definition
(define ALPHA-BASE (- (char->integer #\a) 10))
(define DIGIT-BASE (char->integer #\0))

;; encodes a single byte, returns a two-char hex string
(define (encode-byte value)
  (assert (byte? value) "encode-byte got a non-byte value")

  ;; encode a single hex char
  (define (to-hex v)
    (integer->char
     (if (> v 9)
         (+ ALPHA-BASE v)
         (+ DIGIT-BASE v))))

  (define result (make-string 2 #\0))
  (define-values (hi lo) (quotient/remainder value 16))
  (string-set! result 0 (to-hex hi))
  (string-set! result 1 (to-hex lo))
  result)

(define (encode-byte-test)
  (check-equal? (encode-byte 0) "00" "Encode 0")
  (check-equal? (encode-byte 9) "09" "Encode 9")
  (check-equal? (encode-byte 11) "0b" "Encode 11")
  (check-equal? (encode-byte 15) "0f" "Encode 15")
  (check-equal? (encode-byte 16) "10" "Encode 16")
  (check-equal? (encode-byte 32) "20" "Encode 32")
  (check-equal? (encode-byte 35) "23" "Encode 35")
  (check-equal? (encode-byte 255) "ff" "Encode 255"))

;; decode a single byte, return a byte value
(define (decode-byte str)
  (define str-len (string-length str))
  (assert (<= str-len 2) "decode-byte got a non-byte string")

  ;; decode a single hex char
  (define (to-number ch)
    (cond
     [(char-alphabetic? ch) (- (char->integer ch) ALPHA-BASE)]
     [(char-numeric? ch) (- (char->integer ch) DIGIT-BASE)]
     [else (error (format "~a is not a digit" ch))]))

  (let loop ((result 0) (idx 0))
    (if (>= idx str-len)
        result
        (loop (+ (* result 16) (to-number (string-ref str idx)))
              (add1 idx)))))

(define (decode-byte-test)
  (check-equal? (decode-byte "0") 0 "Decode 0")
  (check-equal? (decode-byte "09") 9 "Decode 09")
  (check-equal? (decode-byte "0b") 11 "Decode 0b")
  (check-equal? (decode-byte "0f") 15 "Decode 0f")
  (check-equal? (decode-byte "10") 16 "Decode 10")
  (check-equal? (decode-byte "20") 32 "Decode 20")
  (check-equal? (decode-byte "23") 35 "Decode 23")
  (check-equal? (decode-byte "ff") 255 "Decode ff"))

;; encode input stream `in', put result in `out'
(define (encode-stream in out)
  (let loop ((value (read-byte in)))
    (unless (eof-object? value)
      (fprintf out "~a " (encode-byte value))
      (loop (read-byte in)))))

(define (encode-stream-test)
  (let ((in (open-input-bytes #"\000\011\013\017\020\040\043\377"))
        (out (open-output-string)))
    (encode-stream in out)
    (check-equal? (get-output-string out)
                  "00 09 0b 0f 10 20 23 ff "
                  "Encode stream")
    (close-input-port in)
    (close-output-port out)))

;; decode input stream `in', put result in `out'
(define (decode-stream in out)
  (define (apply-word f chars)
    (unless (null? chars)
      (f (list->string (reverse chars)))))
  
  (define (with-word in f)
    (let loop ((c (read-char in)) (word '()))
      (cond
       [(eof-object? c) (apply-word f word)]
       [(char-whitespace? c) (apply-word f word) (loop (read-char in) '())]
       [else (loop (read-char in) (cons c word))])))
                                 
  (with-word in (lambda (str) (write-byte (decode-byte str) out))))

(define (decode-stream-test)
  (let ((in (open-input-string "00 9 0b 0f 10 20 23 ff "))
        (out (open-output-bytes)))
    (decode-stream in out)
    (check-equal? (get-output-bytes out)
                  #"\000\011\013\017\020\040\043\377"
                  "Decode stream")
    (close-input-port in)
    (close-output-port out)))

;; skip nbytes bytes from input stream `in'
(define (skip-bytes in nbytes)
  (let loop ((nbytes nbytes) (c (peek-byte in)))
    (unless (or (<= nbytes 0) (eof-object? c))
        (begin (read-byte in) (loop (sub1 nbytes) (peek-byte in))))))

(define (skip-bytes-test)
  (let ((in (open-input-bytes #"\000\011\013\017\020\040\043\377")))
    (skip-bytes in 0)
    (check-equal? (read-byte in) 0 "skip 0 bytes")
    (skip-bytes in 2)
    (check-equal? (read-byte in) 15 "skip 2 bytes")
    (skip-bytes in 5)
    (check-pred eof-object? (read-byte in) "skip out out end")
    (close-input-port in)))

;; main function
(define (main)
  (call-with-input-file (input)
    (lambda (in)
      (skip-bytes in (skip))
      ((if (encode?)
           encode-stream
           decode-stream)
       in
       (current-output-port)))))

;; internal test
(define (run-internal-tests)
  (encode-byte-test)
  (decode-byte-test)
  (encode-stream-test)
  (decode-stream-test)
  (skip-bytes-test))

;; main starts here
(command-line
 #:multi
 [("-s" "--skip") nbytes "skip the first `nbytes' bytes"
  (let ((num (string->number nbytes)))
    (if (and (integer? num) (> num 0))
        (skip (+ num (skip)))
        (error "only accept positive integer offset")))]

 #:once-any
 [("-e" "--encode") "encode input-file to stdout" (encode? #t)]
 [("-d" "--decode") "decode input-file to stdout" (encode? #f)]
 
 #:once-each
 ["--test" "run internal tests" (test #t)]

 #:args (input-file) (input input-file))

(if (test)
    (run-internal-tests)
    (main))
