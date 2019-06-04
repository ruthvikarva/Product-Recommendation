#lang racket

(require racket/set)

; Adjacency graph
(define add-arc!
  (lambda (adj-list n1name n2name weight)
    (cond [(not (hash-has-key? adj-list n1name)) (hash-set! adj-list
n1name (make-hash))])
    (define node1-adjacent-nodes (hash-ref adj-list n1name #f))
    (hash-set! node1-adjacent-nodes n2name weight)))

(define add-bidirectional-arcs!
  (lambda (adj-list n1name n2name weight)
    (add-arc! adj-list n1name n2name weight)
    (add-arc! adj-list n2name n1name weight)))

(define adjacent?
  (lambda (adj-list n1name n2name)
    (define node1-adjacent-nodes (hash-ref adj-list n1name #f))
    (if node1-adjacent-nodes
        (hash-ref node1-adjacent-nodes n2name #f)
        #f)))


(define graph (make-hash))
(define purchase-history (make-hash))
(define products (make-hash))

(define hopLimit
  (lambda (graph start max-hops)
    (define nodes (mutable-set))
    (let loop ((current start)
               (hops 0))
      (define neighbors (hash-keys (hash-ref graph current)))
      (cond ((and (<= hops max-hops) (not (set-member? nodes current)))
             (set-add! nodes current)
             (map (lambda (nnode) (loop nnode (+ hops 1))) neighbors))))
    nodes
    ))

(define dijkstras
  (lambda (graph start target max-hops)
    (define min-dist
      (lambda (queue-set distances)
        (define queue-with-distances (set-map queue-set (lambda (q)
(cons q (hash-ref distances q)))))
        (if (empty? queue-with-distances)
            '()
            (argmin cdr queue-with-distances))))
    (define nodes (hash-keys graph))
    (define distances
      (make-hash
       (map (lambda (x) (if (equal? x start) (cons x 0) (cons x
+inf.f))) nodes)))
    (define queue (hopLimit graph start max-hops))
    (call/cc
     (lambda (cont)
       (let loop ((upair (min-dist queue distances)))
         (cond ((null? upair) (cont #f))
               ((equal? (car upair) target) (cont (hash-ref distances
(car upair))))
               (else
                (define u (car upair))
                (define udist (cdr upair))
                (set-remove! queue u)
                (for ((vpair (hash->list (hash-ref graph u '()))))
                  (define v (car vpair))
                  (define vdist (cdr vpair))
                  (define alt (+ vdist udist))
                  (cond ((< alt (hash-ref distances v)) (hash-set!
distances v alt))))
                (loop (min-dist queue distances)))))))))


(define addUser!
  (lambda (person)
    (hash-set! purchase-history person '())))

(define addSocial!
  (lambda(person1 person2 weight)
    (add-bidirectional-arcs! graph person1 person2 weight)))

(define addPurchase!
  (lambda (person product)
    (cond ((not (hash-has-key? purchase-history person))(addUser! person)))
    (hash-set! purchase-history person (cons product (hash-ref!
purchase-history person #f)))))

(define addProduct!
  (lambda (product category)
    (hash-set! products product category)))

(define get-categories
  (lambda (items)
    (define categories (mutable-set))
    (for ((s items))
      (set-add! categories (hash-ref products s '())))
    categories))

(define get-possible-recommendations
  (lambda (person1 person2)
    (define p1history (hash-ref purchase-history person1 '()))
    (define p2history (hash-ref purchase-history person2 '()))
    (define p1history-categories (get-categories p1history))
    (define p2h-filtered
      (filter (lambda (item) (and(set-member? p1history-categories(hash-ref products item))
                              (not (member item p1history))))p2history))p2h-filtered))

(define recommendProduct
  (lambda (person hops)
    (define givenHopes (hopLimit graph person hops))
    (define group-list '())
    (for/mutable-set ([x (in-mutable-set givenHopes)])
      (cond
         ((>=(set-count(get-possible-recommendations person x))1)
          (set! group-list (cons x group-list)))
    ))

    (if (empty? group-list)
        (println #f)
        (finalize person hops group-list (length group-list)))
    ))


(define finalize
  (lambda (person hops group-list list-count)
    (define count 0)
    (define val 0)
    (define weight (make-hash))
    (for/list ([x (in-list group-list)])
      (cond
        ((and(< count list-count)(equal? count 0))
         (set! val (dijkstras graph person x hops))
         (hash-set! weight val x)
         (set! count (+ count 1))
         )
        ((and(< count list-count)(<(dijkstras graph person x hops) val))
         (set! val (dijkstras graph person x hops))
         (hash-set! weight val x)
         (set! count (+ count 1))
         )
        (else
         (set! count (+ count 1)))
        ))
    (get-possible-recommendations person (hash-ref weight val))
    ))