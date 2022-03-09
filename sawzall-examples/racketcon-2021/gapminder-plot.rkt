#lang racket
(require data-frame
         fancy-app
         plot/pict
         plot/utils
         fast-sequence)

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

(parameterize ([plot-x-label "GDP per capita (USD)"]
               [plot-y-label "Life expectancy (years)"]
               [plot-font-family 'swiss]
               [plot-x-transform log-transform]
               [plot-x-ticks (log-ticks #:scientific? #f)]
               [plot-x-far-ticks no-ticks]
               [plot-y-far-ticks no-ticks]
               [point-sym 'bullet]
               [point-alpha 0.4]
               [plot-pen-color-map 'set1])
  (define tbl (make-hash))
  
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

  #;(for ([(k v) (in-hash tbl)]
        [x (in-range 10)])
    (println (list k (car v))))
  
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
  (time*
   (for ([(x y con) (sequence-filter (lambda (x y con) (and x y)) (in-data-frame gapminder
                                                                                 "gdpPercap"
                                                                                 "lifeExp"
                                                                                 "continent"))])
     (hash-update! tbl con (cons (vector x y) _) null)))
  (time*
   (for ([(x y con) (fast-sequence-filter
                     (lambda (x y con) (and x y))
                     (in-data-frame/syntax gapminder
                                           "gdpPercap"
                                           "lifeExp"
                                           "continent"))])
     (hash-update! tbl con (cons (vector x y) _) null)))
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
  (time*
   (for ([(x y con) (sequence-filter (lambda (x y con) (and x y)) (in-data-frame gapminder
                                                                                 "gdpPercap"
                                                                                 "lifeExp"
                                                                                 "continent"))])
     (hash-update! tbl con (cons (vector x y) _) null)))

  
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
  (time*
   (for ([(x y con) (sequence-map (lambda (x y con) (values (* 2 x) y con))
                                  (sequence-filter (lambda (x y con) (and x y))
                                                   (in-data-frame gapminder
                                                                  "gdpPercap"
                                                                  "lifeExp"
                                                                  "continent")))])
     (hash-update! tbl con (cons (vector x y) _) null)))
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
  (printf "\n")
  
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
                                 #:label con)))))))
