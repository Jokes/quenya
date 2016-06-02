#lang racket

(require xml)

(provide vocab Word)

(struct Word (def full part attrlist) #:transparent)
(struct Affix (def full pre? attrlist) #:transparent)

(define (get-raw infilename)
  (let* ([infile (open-input-file infilename)]
         [r (read-xml infile)])
    (close-input-port infile)
    r))

(define x-vocab (xml->xexpr (document-element (get-raw "elements.xml"))))

(define (attr name ls)
  (let ([ll (memf (λ (m) (equal? (first m) name)) ls)])
    (if ll (first ll) ll)))
(define (dfp- attrs)
  (filter (λ (m) (not (or (equal? (first m) 'def) (equal? (first m) 'full) (equal? (first m) 'part))))
          attrs))

(define (wordize attrs)
  (let ([def (attr 'def attrs)] [full (attr 'full attrs)] [part (attr 'part attrs)])
    (and def full part
         (Word def full part (dfp- attrs)))))
(define (affize attrs pre?)
  (let ([def (attr 'def attrs)] [full (attr 'full attrs)])
    (and def full
         (Affix def full pre? (dfp- attrs)))))

(define vocab
  (filter-map
   (λ (w)
     (match w
       [(list 'word attrs) (wordize attrs)]
       [(list 'prefix attrs) (affize attrs #t)]
       [(list 'suffix attrs) (affize attrs #f)]
       [_ #f]))
   x-vocab))