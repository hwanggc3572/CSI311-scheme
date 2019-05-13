;Project5
;Gyuchan Hwang

#lang racket
(require graph)

(define relation (weighted-graph/undirected '()))
(define people-list '())
(define product-list '())
(define category-list '())

;check if the element x is in the list
(define (contains? list x)
    (cond 
        ((null? list) #f)
        ((eq? (car list) x) #t)
        (else (contains? (cdr list) x))))

;bidirectional weighted relationship to the social web
(define addSocial!
  (lambda (person1 person2 weight)
    (add-edge! relation person1 person2 weight)
    (unless (contains? people-list person1) (set! people-list (cons person1 people-list))) ; check if people list contains person and if it doesn't have, then add it
    (unless (contains? people-list person2) (set! people-list (cons person2 people-list))) ; person2 same
    ))

;notate that a person bought a product
(define addPurchase!
  (lambda (person product)
    (add-edge! relation person product)))

;add a new product to the product catalog associated with a given category
(define addProduct!
  (lambda (product category)
    (add-edge! relation product category)
    (unless (contains? product-list product) (set! product-list (cons product product-list))) ; add product to the product list
    (unless (contains? category-list category) (set! category-list (cons category category-list))) ; add category to the category list
    ))

;check if the person in the people list
(define person?
  (lambda (person)
    (member person people-list)))

;check if the product in the product list
(define product?
  (lambda (product)
    (member product product-list)))

;check if the category in the category list
(define category?
  (lambda (category)
    (member category category-list)))

;get adjacent people (hop 1) of a person
(define getPeople
  (lambda (person)
    (let ([neighbors (get-neighbors relation person)])
      (filter person? neighbors))))

;get purchased list of a person
(define getProducts
  (lambda (person)
    (let ([neighbors (get-neighbors relation person)])
      (filter product? neighbors))))

;get category list of a product
(define getCategory
  (lambda (product)
    (let ([neighbors (get-neighbors relation product)])
      (car (filter category? neighbors)))))

;get weight or relationship
(define relationWeight
  (lambda (person1 person2)
    (edge-weight relation person1 person2)))

;get category list of a person
(define getCategories
  (lambda (person)
    (let ([categories-of-products '()])
      (let ([add-unique-category (lambda (category-candidate)
                                   (unless (member category-candidate categories-of-products)
                                           (set! categories-of-products (cons category-candidate categories-of-products))))])
        (letrec ([get-categories-rec
                  (lambda (product-list)
                    (if (null? product-list) categories-of-products
                        (begin
                          (add-unique-category (getCategory (car product-list)))
                          (get-categories-rec (cdr product-list)))))])
          (get-categories-rec (getProducts person)))))))


;calculate hops between two people
(define cal-hops
  (lambda (person1 person2)
    (if (not (fewest-vertices-path relation person1 person2)) ;if there's no path between two people, hops = 0
        0
        (- (length(fewest-vertices-path relation person1 person2)) 1)))) ;otherwise, hops

;list of related people
(define relatedPeople
  (lambda (person hops)
    (filter (lambda (x) (and (<= (cal-hops person x) hops) (not(= (cal-hops person x) 0)))) people-list))) ; filter elements that (0<x<=hops) 

;calculate the number of same elements of lists
(define numOfSameElement (compose1 length set-intersect))

;common categories
(define commonCategories?
  (lambda (person1 person2)
    (if (> (numOfSameElement (getCategories person1) (getCategories person2)) 0)
        #t
        #f)))

;check if two people didn't buy same thing
(define didntBuy?
  (lambda (person1 person2)
    (if (> (numOfSameElement (getProducts person1) (getProducts person2)) 0)
        #f
        #t)))

;find the best recommendation between two people
(define getBestRecommendation
  (lambda (person1 person2)
    (define p1history (getProducts person1))
    (define p2history (getProducts person2))
    (define p1history-categories (getCategories person1))
    (define p2h-filtered
      (filter (lambda (item) (and
                              (member (getCategory item) p1history-categories)
                              (not (member item p1history))))
              p2history))
    p2h-filtered))


;find the strongest weight with a person in the list
(define strongestWeight
  (lambda (person ls)
    (car(sort (map (lambda (x) (edge-weight relation person x)) ls) <))))

;recommendation (count hops --> check category --> didn't buy yet --> stronger relationship)
(define recommendProduct
  (lambda (person hops)
    (let ([related-group (relatedPeople person hops)]) ;count hops (only related people)
      (let ([related-group (filter (lambda (x) (commonCategories? person x)) related-group)]) ;check category (more related)
        (cond
            ((= (length related-group) 1) (getBestRecommendation person (car related-group))) ;if only one person in the related group, recommend the product
            ((= (length related-group) 0) #f) ;if there's no person who meets these requirements
            (else (getBestRecommendation person (car (memf (lambda (x) (= (edge-weight relation person x) (strongestWeight person related-group))) related-group))))
              ;find the strongest weight and a person who has that weight, and get that person's product
              )))))

          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;execution
;weight: Parent-Child(1), Romantic relationship(3), Friends(5), Neighbor(7), Club membership(10), Co-workers(20) 
(addSocial! 'Bob 'Sue 5)
(addSocial! 'Sue 'Andrea 10)
(addSocial! 'Andrea 'Katie 1)
(addSocial! 'Katie 'Alex 10)
(addSocial! 'Phil 'Jin 10)
(addSocial! 'Bob 'Jonathan 20)
(addSocial! 'Sue 'Amy 1)
(addSocial! 'Bob 'Annie 7)


(addProduct! 'iPhone 'Electronics)
(addProduct! 'Macbook 'Electronics)
(addProduct! 'Toaster 'Homewares)
(addProduct! 'Blender 'Homewares)
(addProduct! 'Microwave 'Homewares)
(addProduct! 'Airconditioner 'Homewares)
(addProduct! 'Bread 'Food)
(addProduct! 'Rice 'Food)
(addProduct! 'Egg 'Food)
(addProduct! 'Android 'Electronics)


(addPurchase! 'Bob 'Toaster)
(addPurchase! 'Bob 'Macbook)
(addPurchase! 'Sue 'Blender)
(addPurchase! 'Andrea 'iPhone)
(addPurchase! 'Andrea 'Rice)
(addPurchase! 'Katie 'Egg)
(addPurchase! 'Katie 'Airconditioner)
(addPurchase! 'Jonathan 'Bread)
(addPurchase! 'Amy 'Macbook)
(addPurchase! 'Amy 'Microwave)
(addPurchase! 'Annie 'Android)

people-list
product-list
category-list

(relatedPeople 'Bob 2)
(filter (lambda (x) (commonCategories? 'Bob x)) (relatedPeople 'Bob 2))
(recommendProduct 'Bob 2)

(relatedPeople 'Sue 2)
(filter (lambda (x) (commonCategories? 'Sue x)) (relatedPeople 'Sue 2))
(recommendProduct 'Sue 2)

(relatedPeople 'Andrea 3)
(filter (lambda (x) (commonCategories? 'Andrea x)) (relatedPeople 'Andrea 3))
(recommendProduct 'Andrea 3)

(relatedPeople 'Alex 2)
(filter (lambda (x) (commonCategories? 'Alex x)) (relatedPeople 'Alex 2))
(recommendProduct 'Alex 2)

;(getPeople 'Bob)
;(closestPerson 'Bob)
;(getProducts 'Bob)
;(getCategories 'Bob)
;(getCategory 'iPhone)
;(relatedPeople 'Bob 2)
;(commonCategories? 'Sue 'Bob)
;(commonCategories? 'Andrea 'Bob)
;(commonCategories? 'Jonathan 'Bob)
;(numOfSameElement (getCategories 'Bob) (getCategories 'Sue))
;(get-edges relation)
;(cal-hops 'Bob 'Bob)
;(cal-hops 'Bob 'Sue)
;(cal-hops 'Bob 'Andrea)
;(cal-hops 'Bob 'Jonathan)
;(cal-hops 'Bob 'Katie)
;(cal-hops 'Bob 'Alex)
;(cal-hops 'Bob 'Phil)
;(cal-hops 'Bob 'Jin)
;(relatedPeople 'Bob 2)
;(relatedPeople 'Sue 2)