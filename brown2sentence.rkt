#lang racket

(require 2htdp/batch-io)


(define DATA_DIR "data/brown/")


(define cats (read-lines (string-append DATA_DIR "cats.txt")))

(define (cat-filename cat)
  (first (string-split cat)))

(define filenames (map cat-filename cats))



(define (string-join-\n strs)
  (string-join strs "\n"))

(define (line-words line)
  (map string-downcase
    (filter (lambda (token) (regexp-match? #px"\\w+" token))
      (map (lambda (token) (first (string-split token "/"))) 
        (string-split line)))))

(write-file "data/brown-sentence.txt"
  (string-join-\n
    (for/list ([filename filenames])
      (string-join-\n
        (map string-join
          (map line-words
            (map string-trim
              (filter non-empty-string?
                (read-lines (string-append DATA_DIR filename))))))))))
