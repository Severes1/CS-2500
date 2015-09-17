;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Pong) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require graphics/graphics)

(define-struct pair (x y))
(define (add-pair p q)
  (make-pair (+ (pair-x p) (pair-x q)) (+ (pair-y p) (pair-y q))))

(define SCENE (empty-scene 600 300))
(define BALL (circle 5 "solid" "black"))
(define PADDLE (rectangle 7 40 "solid" "black"))
 
(define (check-bounds p) 
  (make-pair (pair-x p)
             (max (min (pair-y p) (- (image-height SCENE) (- (/ (image-height BALL) 2) 1))) (/ (image-height BALL) 4))))
   
(define-struct world (ball-speed score1 score2 ball-pos ball-vel paddle1-pos paddle2-pos))

(define (drawWorld w)
  (place-image PADDLE (pair-x (world-paddle1-pos w)) (pair-y (world-paddle1-pos w))
  (place-image PADDLE (pair-x (world-paddle2-pos w)) (pair-y (world-paddle2-pos w))
  (place-image BALL (pair-x (world-ball-pos w)) (pair-y (world-ball-pos w))
  (place-image (text (number->string (world-score1 w)) 20 "black") (- (/ (image-width SCENE) 2) 50) 50
  (place-image (text (number->string (world-score2 w)) 20 "black") (+ (/ (image-width SCENE) 2) 50) 50
   SCENE))))))

(define (check-score2 w) 
  (cond
    [(and (not (in-play? w)) (< (pair-x (world-ball-pos w)) 0)) (add1 (world-score2 w))]
    [else (world-score2 w)]))
(define (check-score1 w) 
  (cond
    [(and (not (in-play? w)) (> (pair-x (world-ball-pos w)) (image-width SCENE))) (add1 (world-score1 w))]
    [else (world-score1 w)]))
 
(define (in-play? w)
  (and
   (>= (pair-x (world-ball-pos w)) (- 0 (/ (image-height BALL) 2)))
   (<= (pair-x (world-ball-pos w)) (+ (/ (image-height BALL) 2) (image-width SCENE)))))
   ;;(>= (pair-y (world-ball-pos w)) (/ (image-height BALL) 2))
   ;;(<= (pair-y (world-ball-pos w)) (+ (/ (image-height BALL) 2) (image-height SCENE)))))

(define (get-complement-pair s x)
  (make-pair x (sqrt (- (sqr s) (sqr x)))))

(define (nextWorld w)
  (make-world
   (* 5 (+ 2 (/ (+ (world-score1 w) (world-score2 w)) 2)))
   (check-score1 w)
   (check-score2 w) 
   ;; Add velocity to position
   (cond
     [(in-play? w) (check-bounds (add-pair (world-ball-pos w) (world-ball-vel w)))]
     [else (make-pair (/ (image-width SCENE) 2) (/ (image-height SCENE) 2))])
                   
   ;; Bounce off top and bottom wall
   (cond
     [(not (in-play? w)) (get-complement-pair (world-ball-speed w) (random (round (- (* (world-ball-speed w) 2) (world-ball-speed w)))))]
     [else
      (make-pair
       ;; X velocity calculation
       (cond
         [(or (and (< (pair-x (world-ball-vel w)) 0) ;; Ball is traveling left
                   (< (pair-x (world-ball-pos w)) (+ (pair-x (world-paddle1-pos w)) (/ (image-width BALL) 2))) ;; Ball position is at left bound
                   (> (pair-x (world-ball-pos w)) (- (pair-x (world-paddle1-pos w)) (abs (pair-x (world-ball-vel w))))) 
                   (< (pair-y (world-ball-pos w)) (+ (pair-y (world-paddle1-pos w)) (/ (image-height PADDLE) 2))) ;; Ball is within paddle y
                   (> (pair-y (world-ball-pos w)) (- (pair-y (world-paddle1-pos w)) (/ (image-height PADDLE) 2)))) ;; ^^^
              (and (> (pair-x (world-ball-vel w)) 0) ;; Ball is traveling right
                   (> (pair-x (world-ball-pos w))(- (pair-x (world-paddle2-pos w)) (/ (image-width BALL) 2))) ;; Ball is at right bound
                   (< (pair-x (world-ball-pos w)) (+ (pair-x (world-paddle2-pos w))  (abs (pair-x (world-ball-vel w))))) 
                   (< (pair-y (world-ball-pos w)) (+ (pair-y (world-paddle2-pos w)) (/ (image-height PADDLE) 2))) ;; Ball is within paddle y
                   (> (pair-y (world-ball-pos w)) (- (pair-y (world-paddle2-pos w)) (/ (image-height PADDLE) 2))))) ;; ^^
          (* -1 (pair-x (world-ball-vel w)))] ;; Flip x direction
         [else (pair-x (world-ball-vel w))])
       ;; Y velocity calculation:
       (cond  
         [(or (and (< (pair-y (world-ball-vel w)) 0) (< (pair-y (world-ball-pos w)) (/ (image-height BALL) 2)))
              (and (> (pair-y (world-ball-vel w)) 0) (> (pair-y (world-ball-pos w)) (- (image-height SCENE) (/ (image-height BALL) 2)))))
          (* -1 (pair-y (world-ball-vel w)))] 
         [else (pair-y (world-ball-vel w))]))])
      ;; Paddle position
      (make-pair (pair-x (world-paddle1-pos w)) (pair-y (world-ball-pos w)))
      (world-paddle2-pos w)))
   
(define (paddle-controls w a-key)
  (cond
    [(key=? a-key "up")    (make-world (world-ball-speed w) (world-score1 w) (world-score2 w) (world-ball-pos w) (world-ball-vel w) (world-paddle1-pos w) (make-pair (pair-x (world-paddle2-pos w)) (+ (cond [(> (pair-y (world-paddle2-pos w)) 0) -5] [else 0]) (pair-y (world-paddle2-pos w)))))]
    [(key=? a-key "down")  (make-world (world-ball-speed w) (world-score1 w) (world-score2 w) (world-ball-pos w) (world-ball-vel w) (world-paddle1-pos w) (make-pair (pair-x (world-paddle2-pos w)) (+ (cond [(< (pair-y (world-paddle2-pos w)) (image-height SCENE)) 5] [else 0]) (pair-y (world-paddle2-pos w)))))]
    [else w]))
 
(define WORLD0 (make-world 10 0 0 (make-pair 500 150) (make-pair -5 -5) (make-pair 50 150) (make-pair 550 150)))

(big-bang WORLD0 (on-tick nextWorld) (to-draw drawWorld) (on-key paddle-controls))