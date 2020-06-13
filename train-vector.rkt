#lang racket

(require math/array)
(require math/matrix)
(require 2htdp/batch-io)


(define INITIAL_RANGE 0.1)
(define LEARNING_RATE 0.01)


(define (string-join-\n strs)
  (string-join strs "\n"))


(define (shape mat) (list (matrix-num-rows mat) (matrix-num-cols mat)))

(define (array-set-row! arr row vals)
  (for ([col (range (second (shape vals)))])
    (array-set! arr (vector row col) (matrix-ref vals 0 col))))


(define (sum-matrix mat)
  (let* ([s (shape mat)]
         [o (apply make-matrix (flatten (list (reverse s) 1)))])
    (matrix-ref (matrix* mat o) 0 0)))
  
(define (softmax mat)
  (let* ([exp-mat (matrix-map exp mat)]
         [exp-sum (sum-matrix exp-mat)])
    (matrix-map (lambda (v) (/ v exp-sum)) exp-mat)))


(define (initial-value) (- (/ INITIAL_RANGE 2) (* (random) INITIAL_RANGE)))

(define (sample-x1 sample) (first sample))
(define (sample-x2 sample) (second sample))
(define (sample-data sample) (list (first sample) (second sample)))
(define (sample-y sample) (third sample))


(define (train-vectors samples 
    #:vector-size [vector-size 10])

  (define total-words (remove-duplicates (flatten (map sample-data samples))))
  (define words-num (length total-words))

  (define word-index
    (apply hash (flatten
      (for/list ([word total-words]
              [i (range words-num)])
        (list word i)))))

  (define (sample->index sample)
    (list 
      (hash-ref word-index (sample-x1 sample))
      (hash-ref word-index (sample-x2 sample))
      (string->number (sample-y sample))))

  (define weight (array->mutable-array 
    (build-matrix words-num vector-size (lambda (r c) (initial-value)))))

  (define (word-weight x) (matrix-row weight x))

  (define (sample-y-hat index-sample)
    (let ([x1 (sample-x1 index-sample)]
          [x2 (sample-x2 index-sample)])
      (matrix-ref (softmax (matrix* (word-weight x1) (matrix-transpose weight))) 0 x2)))

  (for ([index-sample (map sample->index samples)])
    (let* ([x1 (sample-x1 index-sample)]
           [x2 (sample-x2 index-sample)]
           [w1 (word-weight x1)]
           [w2 (word-weight x2)]
           [y (sample-y index-sample)]
           [y-hat (sample-y-hat index-sample)])
      (array-set-row! weight x1 
        (matrix- w1 (matrix-scale w2 (* LEARNING_RATE (- y-hat y)))))
      (array-set-row! weight x2 
        (matrix- w2 (matrix-scale w1 (* LEARNING_RATE (- y-hat y)))))))

  (hash-map word-index 
    (lambda (word index) (list word (matrix->list (word-weight index))))))


(define samples 
  (map string-split (read-lines "data/samples-1000.txt")))

(define train-result (train-vectors samples))

(define output "data/word2vec.txt")
(write-file output
  (string-join-\n 
    (map (lambda (ret) (string-join (list (first ret) (string-join (map number->string (second ret))))))
      train-result)))
(println (format "~a saved." output))