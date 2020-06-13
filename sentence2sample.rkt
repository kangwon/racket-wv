#lang racket

(require 2htdp/batch-io)


(define NS_EXPONENT 0.75)
(define SAMPLE_RATE 0.001)


(define (nth lst counter)
  (cond ((empty? lst) (error 'nth "index out of bounds"))
        ((= counter 0) (first lst))
        (else (nth (rest lst) (- counter 1)))))

(define (random-choice lst)
  (nth lst (exact-floor (* (random) (length lst)))))

(define (repeat val n)
  (map (lambda (i) val) (range n)))

(define (string-join-\n strs)
  (string-join strs "\n"))


(define (sentences-sample sentences window-size negative-size)
  (define total-words (length (flatten sentences)))

  (define frequency
    (map (lambda (words) (list (first words) (length words)))
      (group-by identity 
        (flatten sentences))))

  (define positive-sample
    (let ()
      (define (p cnt)
        (let* ([z (/ cnt total-words)])
          (min 1 (* (+ (sqrt (/ z SAMPLE_RATE)) 1) (/ SAMPLE_RATE z)))))

      (define p-list (map (lambda (freq) (list (first freq) (p (second freq)))) frequency))
      (define ht (apply hash (flatten p-list)))
      
      (lambda (sentence) 
        (filter (lambda (word) (< (random) (hash-ref ht word))) sentence))))

  (define negative
    (let ()
      (define words
        (append-map 
          (lambda (freq) 
            (repeat (first freq) (round (expt (last freq) NS_EXPONENT))))
          frequency))
      (lambda () (random-choice words))))

  (define (sentence-sample sentence)
    (define (positive-before pos)
      (let ([word (nth sentence pos)])
        (for/list ([i (range (max (- pos window-size) 0) pos)])
          (list word (nth sentence i) 1))))

    (define (positive-after pos)
      (let* ([word (nth sentence pos)]
            [len (length sentence)])
        (for/list ([i (range (+ pos 1) (min (+ pos window-size) len))])
          (list word (nth sentence i) 1))))

    (define (positive-at pos)
      (append (positive-before pos) (positive-after pos)))

    (define (negative-at pos)
      (let ([word (nth sentence pos)])
      (for/list ([i (range negative-size)])
          (list word (negative) 0))))

    (define (sample-at pos)
      (append (positive-at pos) (negative-at pos)))

    (append-map sample-at (range (length sentence))))
  
  (append-map (lambda (sentence) 
    (sentence-sample (positive-sample sentence)))
    sentences))


(define corpus (read-lines "data/brown-sentence.txt"))
(define sentences (map string-split corpus))
(println (format "~a sentences loaded." (length sentences)))

(define window-size 5)
(define negative-size 5)
(define samples (sentences-sample sentences window-size negative-size))
(println (format "~a samples build." (length samples)))

(define output "data/samples.txt")
(write-file output
  (string-join-\n 
    (map (lambda (s) (apply format "~a ~a ~a" s)) samples)))
(println (format "~a saved." output))
