#lang racket
(require data-frame
         fancy-app
         #;plot/pict
         #;plot/utils
         fast-sequence
         data/collection
         sawzall
         (lib "sawzall/grouped-df")
         "nest.rkt")

(define gapminder (df-read/csv "../data/all_gapminder.csv"))

(define fit
  (df-least-squares-fit gapminder "gdpPercap"
                        "lifeExp" #:mode 'log))

(define ITER-CT 2000)
  
(define-syntax-rule (time* expr)
  (begin
    (collect-garbage)
    (collect-garbage)
    (let-values
        ([(results t real-t gc-t)
          (time-apply
           (lambda ()
             (for ([i (in-range ITER-CT)])
               expr))
           null)])
      (printf "~a ms\n" t))))

#;(parameterize () #;([plot-x-label "GDP per capita (USD)"]
                      [plot-y-label "Life expectancy (years)"]
                      [plot-font-family 'swiss]
                      [plot-x-transform log-transform]
                      [plot-x-ticks (log-ticks #:scientific? #f)]
                      [plot-x-far-ticks no-ticks]
                      [plot-y-far-ticks no-ticks]
                      [point-sym 'bullet]
                      [point-alpha 0.4]
                      [plot-pen-color-map 'set1]))
(define tbl (make-hash))

;; ======================================= Printing
(when #f
  (for ([(x y con) (in-data-frame gapminder
                                  "gdpPercap"
                                  "lifeExp"
                                  "continent")]
        #:when (and x y))
    (hash-update! tbl con (cons (vector x y) _) null))
  
  (for ([(k v) (in-hash tbl)]
        [x (in-range 10)])
    (println (list k (car v))))

  #;(for ([(k v) (in-hash tbl)]
          [x (in-range 10)]
          #:when #t
          [y (in-range (exact-round (vector-ref (car v) 1)))]
          [z (in-range 2)])
      (println (list k (car v) y)))

  #;(println (for/sum ([(k v) (in-hash tbl)]
                       [x (in-range 10)]
                       #:when #t
                       [y (let ([a (exact-round (vector-ref (car v) 1))])
                            (range a (+ 2 a)))])
               y))

  #;(println (for/sum ([x (do/sequence ([(k v) (in-hash tbl)]
                                        [x (in-range 10)]
                                        #:when #t
                                        [y (let ([a (exact-round (vector-ref (car v) 1))])
                                             (range a (+ 2 a)))])
                            y)])
               x))

  #;(for ([l (do/sequence ([(v1 v2 k) (in-data-frame gapminder
                                                     "gdpPercap"
                                                     "lifeExp"
                                                     "continent")]
                           [x (in-range 5)]
                           #:when #t
                           [y (let ([a (exact-round v2)])
                                (range a (+ 2 a)))])
               (list k (vector v1 v2) y))])
      (println l))
    
  #;(println (for/sum ([x (do/sequence ([(x y con) (in-data-frame gapminder
                                                                  "gdpPercap"
                                                                  "lifeExp"
                                                                  "continent")]
                                        [a (in-range 5)]
                                        #:when #t
                                        [b (let ([c (exact-round y)])
                                             (range c (+ 2 c)))])
                            b)])
               x)))

;; ======================================= Nesting
(when #f
  (for ([(x y con) (in-data-frame gapminder
                                  "gdpPercap"
                                  "lifeExp"
                                  "continent")]
        [i (in-naturals)])
    (hash-update! tbl con (cons (vector x y i) _) null))
  
  (for ([(k l) (in-hash tbl)]
        [x (in-range 10)])
    (println (list k (argmin (lambda (v) (vector-ref v 2)) l)))))

#;(println (df-series-names gapminder))
;; What is the sequence of triples of two countries from
;; the same continent and a year where the first country
;; has a higher population than the second country in that year?

(when #t
  (set! ITER-CT 10)

  (time*
   (for/list ([l (do/sequence ([(country1 year1 continent1 pop1)
                                (in-data-frame gapminder
                                               "country"
                                               "year"
                                               "continent"
                                               "pop")]
                               #:when #t
                               [(country2 year2 continent2 pop2)
                                (in-data-frame gapminder
                                               "country"
                                               "year"
                                               "continent"
                                               "pop")]
                               #:when (and (equal? continent1 continent2)
                                           (> pop1 pop2)
                                           (equal? year1 year2)))
                   (list country1 country2 year1))])
     l))

  (df-add-index! gapminder "yearInd" "year" <)

  (time*
   (for/list ([l (do/sequence ([(country1 year1 continent1 pop1)
                                (in-data-frame/by-index gapminder
                                                        #:index "yearInd"
                                                        "country"
                                                        "year"
                                                        "continent"
                                                        "pop")]
                               #:when #t
                               [(country2 year2 continent2 pop2)
                                (in-data-frame/by-index gapminder
                                                        #:index "yearInd"
                                                        #:from year1
                                                        #:to   year1
                                                        "country"
                                                        "year"
                                                        "continent"
                                                        "pop")]
                               #:when (and (equal? continent1 continent2)
                                           (> pop1 pop2)))
                   (list country1 country2 year1))])
     l))

  (time*
   (for/list ([l (do/sequence ([(country1 year1 continent1 pop1)
                                (in-data-frame/by-index/syntax gapminder
                                                               #:index "yearInd"
                                                               "country"
                                                               "year"
                                                               "continent"
                                                               "pop")]
                               #:when #t
                               [(country2 year2 continent2 pop2)
                                (in-data-frame/by-index/syntax gapminder
                                                               #:index "yearInd"
                                                               #:from year1
                                                               #:to   year1
                                                               "country"
                                                               "year"
                                                               "continent"
                                                               "pop")]
                               #:when (and (equal? continent1 continent2)
                                           (> pop1 pop2)))
                   (list country1 country2 year1))])
     l))

  (time*
   (for/list ([l (do/sequence ([(country1 year1 continent1 pop1)
                                (in-data-frame/by-index1/syntax gapminder
                                                                #:index "yearInd"
                                                                "country"
                                                                "year"
                                                                "continent"
                                                                "pop")]
                               #:when #t
                               [(country2 year2 continent2 pop2)
                                (in-data-frame/by-index1/syntax gapminder
                                                                #:index "yearInd"
                                                                #:from year1
                                                                #:to   year1
                                                                "country"
                                                                "year"
                                                                "continent"
                                                                "pop")]
                               #:when (and (equal? continent1 continent2)
                                           (> pop1 pop2)))
                   (list country1 country2 year1))])
     l))

  (time*
   (for/list ([l (do/sequence ([(country1 year1 continent1 pop1)
                                (in-data-frame/by-index2/syntax gapminder
                                                                #:index "yearInd"
                                                                "country"
                                                                "year"
                                                                "continent"
                                                                "pop")]
                               #:when #t
                               [(country2 year2 continent2 pop2)
                                (in-data-frame/by-index2/syntax gapminder
                                                                #:index "yearInd"
                                                                #:from year1
                                                                #:to   year1
                                                                "country"
                                                                "year"
                                                                "continent"
                                                                "pop")]
                               #:when (and (equal? continent1 continent2)
                                           (> pop1 pop2)))
                   (list country1 country2 year1))])
     l))

  (df-add-index*! gapminder "yearContinentInd" (list "year" "continent") (list < string<?))

  (time*
   (for/list ([l (do/sequence ([(country1 year1 continent1 pop1)
                                (in-data-frame/by-index* gapminder
                                                         #:index "yearContinentInd"
                                                         "country"
                                                         "year"
                                                         "continent"
                                                         "pop")]
                               #:when #t
                               [(country2 year2 continent2 pop2)
                                (in-data-frame/by-index* gapminder
                                                         #:index "yearContinentInd"
                                                         #:from (list year1 continent1)
                                                         #:to   (list year1 continent1)
                                                         "country"
                                                         "year"
                                                         "continent"
                                                         "pop")]
                               #:when (and (equal? continent1 continent2)
                                           (> pop1 pop2)))
                   (list country1 country2 year1))])
     l))

  (time*
   (for/list ([l (do/sequence ([(country1 year1 continent1 pop1)
                                (in-data-frame/by-index*/syntax gapminder
                                                                #:index "yearContinentInd"
                                                                "country"
                                                                "year"
                                                                "continent"
                                                                "pop")]
                               #:when #t
                               [(country2 year2 continent2 pop2)
                                (in-data-frame/by-index*/syntax gapminder
                                                                #:index "yearContinentInd"
                                                                #:from (list year1 continent1)
                                                                #:to   (list year1 continent1)
                                                                "country"
                                                                "year"
                                                                "continent"
                                                                "pop")]
                               #:when (and (equal? continent1 continent2)
                                           (> pop1 pop2)))
                   (list country1 country2 year1))])
     l))

  (time*
   (for/list ([l (do/sequence ([(country1 year1 continent1 pop1)
                                (in-data-frame/by-index*1/syntax gapminder
                                                                 #:index "yearContinentInd"
                                                                 "country"
                                                                 "year"
                                                                 "continent"
                                                                 "pop")]
                               #:when #t
                               [(country2 year2 continent2 pop2)
                                (in-data-frame/by-index*1/syntax gapminder
                                                                 #:index "yearContinentInd"
                                                                 #:from (list year1 continent1)
                                                                 #:to   (list year1 continent1)
                                                                 "country"
                                                                 "year"
                                                                 "continent"
                                                                 "pop")]
                               #:when (and (equal? continent1 continent2)
                                           (> pop1 pop2)))
                   (list country1 country2 year1))])
     l)))

;; ----------------------
#;(for ([v (in-list (grouped-data-frame-group-indices (group-with gapminder "continent")))]
        #:when #t
        [i (in-vector v)]
        #:when #t
        [(x y con) (in-data-frame/syntax (reorder gapminder "continent")
                                         #:start (ivl-beg i)
                                         #:stop  (ivl-end i)
                                         "gdpPercap"
                                         "lifeExp"
                                         "continent")]
        [n (in-range 3)])
    (println (list con (vector x y))))
  
(when #f
  (set! ITER-CT 20)
  ;; Sequence[Sequence[(values Real Real String Integer]]
  (time*                                                 ;; Real Real String -> Sequence[(values Real Real String Integer)]
   (for ([(x y con b) (in-concat-sequences (sequence-map (lambda (x y con)
                                                           (sequence-map (lambda (z) (values x y con z))
                                                                         (let ([c (exact-round y)])
                                                                           (in-range c (+ 100 c)))))
                                                         ;; Sequence[(values Real Real String)]
                                                         (in-data-frame gapminder
                                                                        "gdpPercap"
                                                                        "lifeExp"
                                                                        "continent")))])
     (hash-update! tbl con (cons (vector x y b) _) null)))
    
  (set! tbl (make-hash))

  (set! tbl (make-hash))
    
  (time*
   (for ([(x y con b) (do/sequence ([s (sequence-map (lambda (x y con)
                                                       (sequence-map (lambda (z) (values x y con z))
                                                                     (let ([c (exact-round y)])
                                                                       (in-range c (+ 100 c)))))
                                                     (in-data-frame gapminder
                                                                    "gdpPercap"
                                                                    "lifeExp"
                                                                    "continent"))]
                                    #:when #t
                                    [(x y con b) s])
                        (values x y con b))])
     (hash-update! tbl con (cons (vector x y b) _) null)))

  (time*
   (for ([(x y con b) (do/sequence ([s (do/sequence ([(x y con) (in-data-frame/syntax gapminder
                                                                                      "gdpPercap"
                                                                                      "lifeExp"
                                                                                      "continent")])
                                         (sequence-map (lambda (z) (values x y con z))
                                                       (let ([c (exact-round y)])
                                                         (in-range c (+ 100 c)))))]
                                    #:when #t
                                    [(x y con b) s])
                        (values x y con b))])
     (hash-update! tbl con (cons (vector x y b) _) null)))

  #;(time*
     (for ([(x y con b) (do/sequence ([(x y con) (in-data-frame gapminder
                                                                "gdpPercap"
                                                                "lifeExp"
                                                                "continent")]
                                      #:when #t
                                      [b (let ([c (exact-round y)])
                                           (in-range c (+ 100 c)))])
                          (values x y con b))])
       (hash-update! tbl con (cons (vector x y b) _) null)))
    
  (time*
   (for ([(x y con b) (do/sequence ([(x y con) (in-data-frame gapminder
                                                              "gdpPercap"
                                                              "lifeExp"
                                                              "continent")]
                                    #:when #t
                                    [c (in-value (exact-round y))]
                                    #:when #t
                                    [b (in-range c (+ 100 c))])
                        (values x y con b))])
     (hash-update! tbl con (cons (vector x y b) _) null)))

  (time*
   (for ([v (for/sequence ([(x y con) (in-data-frame gapminder
                                                     "gdpPercap"
                                                     "lifeExp"
                                                     "continent")]
                           #:when #t
                           [c (in-value (exact-round y))]
                           #:when #t
                           [b (in-range c (+ 100 c))])
              (vector x y con b))])
     (let ([x (vector-ref v 0)]
           [y (vector-ref v 1)]
           [con (vector-ref v 2)]
           [b (vector-ref v 3)])
       (hash-update! tbl con (cons (vector x y b) _) null)))))
  
;; ======================================= Mapping and filtering
(when #f
  (set! ITER-CT 2000)
  (set! tbl (make-hash))
    
  (time*
   (for ([(x y con) (in-data-frame gapminder
                                   "gdpPercap"
                                   "lifeExp"
                                   "continent")]
         #:when (and x y))
     (hash-update! tbl con (cons (vector x y) _) null)))

  (set! tbl (make-hash))
  
  (time*
   (for ([(x y con) (in-data-frame/syntax gapminder
                                          "gdpPercap"
                                          "lifeExp"
                                          "continent")]
         #:when (and x y))
     (hash-update! tbl con (cons (vector x y) _) null)))
  (printf "\n")
  
  ;; -------------------------------------
  (when #f
    (set! tbl (make-hash))
  
    (time*
     (for ([(x y con) (in-data-frame gapminder
                                     "gdpPercap"
                                     "lifeExp"
                                     "continent")]
           #:when (and x y))
       (hash-update! tbl con (cons (vector x y) _) null)))

    (set! tbl (make-hash))
  
    (time*
     (for ([(x y con) (do/sequence ([(gdpPercap) (in-value (df-get-series gapminder "gdpPercap"))]
                                    [(lifeExp) (in-value (df-get-series gapminder "lifeExp"))]
                                    [(continent) (in-value (df-get-series gapminder "continent"))]
                                    #:when #t
                                    [pos (in-range 0 (df-row-count gapminder) (if (<= 0 (df-row-count gapminder)) 1 -1))])
                        (values (series-ref gdpPercap pos) (series-ref lifeExp pos) (series-ref continent pos)))]
           #:when (and x y))
       (hash-update! tbl con (cons (vector x y) _) null)))
    (printf "\n"))

  ;; -------------------------------------
  (set! tbl (make-hash))
  
  (time*
   (for ([(x y con) (in-data-frame gapminder
                                   "gdpPercap"
                                   "lifeExp"
                                   "continent")]
         #:when (and x y))
     (hash-update! tbl con (cons (vector x y) _) null)))

  (set! tbl (make-hash))
  
  (time*
   (for ([(x y con) (do/sequence ([(gdpPercap) (in-value (df-get-series gapminder "gdpPercap"))]
                                  [(lifeExp) (in-value (df-get-series gapminder "lifeExp"))]
                                  [(continent) (in-value (df-get-series gapminder "continent"))]
                                  [(stop) (in-value (df-row-count gapminder))]
                                  #:when #t
                                  [pos (in-range 0 stop (if (<= 0 stop) 1 -1))])
                      (values (series-ref gdpPercap pos) (series-ref lifeExp pos) (series-ref continent pos)))]
         #:when (and x y))
     (hash-update! tbl con (cons (vector x y) _) null)))
  (printf "\n")

  ;; -------------------------------------
  (set! tbl (make-hash))
    
  (time*
   (for ([(x y con) (sequence-filter (lambda (x y con) (and x y)) (in-data-frame gapminder
                                                                                 "gdpPercap"
                                                                                 "lifeExp"
                                                                                 "continent"))])
     (hash-update! tbl con (cons (vector x y) _) null)))

  (set! tbl (make-hash))
    
  (time*
   (for ([(x y con) (fast-sequence-filter
                     (lambda (x y con) (and x y))
                     (in-data-frame/syntax gapminder
                                           "gdpPercap"
                                           "lifeExp"
                                           "continent"))])
     (hash-update! tbl con (cons (vector x y) _) null)))

  (set! tbl (make-hash))
    
  (time*
   (for ([(x y con) (do/sequence ([(x y con) (in-data-frame/syntax gapminder
                                                                   "gdpPercap"
                                                                   "lifeExp"
                                                                   "continent")]
                                  #:when (and x y))
                      (values x y con))])
     (hash-update! tbl con (cons (vector x y) _) null)))
  (printf "\n")
  
  ;; -------------------------------------
  (set! tbl (make-hash))
    
  (time*
   (for ([(x y con) (sequence-filter (lambda (x y con) (and x y)) (in-data-frame gapminder
                                                                                 "gdpPercap"
                                                                                 "lifeExp"
                                                                                 "continent"))])
     (hash-update! tbl con (cons (vector x y) _) null)))

  (set! tbl (make-hash))
    
  (time*
   (for ([(x y con) (do/sequence ([(gdpPercap) (in-value (df-get-series gapminder "gdpPercap"))]
                                  [(lifeExp) (in-value (df-get-series gapminder "lifeExp"))]
                                  [(continent) (in-value (df-get-series gapminder "continent"))]
                                  [(stop) (in-value (df-row-count gapminder))]
                                  #:when #t
                                  [pos (in-range 0 stop (if (<= 0 stop) 1 -1))]
                                  #:when #t
                                  [x (in-value (series-ref gdpPercap pos))]
                                  [y (in-value (series-ref lifeExp pos))]
                                  #:when (and x y))
                      (values x y (series-ref continent pos)))])
     (hash-update! tbl con (cons (vector x y) _) null)))
  (printf "\n")

  ;; -------------------------------------
  (set! tbl (make-hash))
    
  (time*
   (for ([(x y con) (sequence-map (lambda (x y con) (values (* 2 x) y con))
                                  (sequence-filter (lambda (x y con) (and x y))
                                                   (in-data-frame gapminder
                                                                  "gdpPercap"
                                                                  "lifeExp"
                                                                  "continent")))])
     (hash-update! tbl con (cons (vector x y) _) null)))

  (set! tbl (make-hash))
    
  (time*
   (for ([(x y con) (do/sequence ([(gdpPercap) (in-value (df-get-series gapminder "gdpPercap"))]
                                  [(lifeExp) (in-value (df-get-series gapminder "lifeExp"))]
                                  [(continent) (in-value (df-get-series gapminder "continent"))]
                                  [(stop) (in-value (df-row-count gapminder))]
                                  #:when #t
                                  [pos (in-range 0 stop (if (<= 0 stop) 1 -1))]
                                  #:when #t
                                  [x (in-value (series-ref gdpPercap pos))]
                                  [y (in-value (series-ref lifeExp pos))]
                                  #:when (and x y))
                      (values (* 2 x) y (series-ref continent pos)))])
     (hash-update! tbl con (cons (vector x y) _) null)))
  (printf "\n"))
  
#;(for ([(x y con) (in-data-frame gapminder
                                  "gdpPercap"
                                  "lifeExp"
                                  "continent")]
        #:when (and x y))
    (hash-update! tbl con (cons (vector x y) _) null))

#;(plot
   (cons (function fit #:width 3 #:color 'blue)
         (let ([color-n -1])
           (hash-map tbl
                     (lambda (con pts)
                       (set! color-n (add1 color-n))
                       (points pts
                               #:color (->pen-color color-n)
                               #:label con))))))
