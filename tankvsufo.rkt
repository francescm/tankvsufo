;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tankvsufo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; constants

(define SLUG (rectangle 2 6 "solid" "black"))
(define BOMB (circle 3 "solid" "black"))

(define TANK (above
              (beside
               (rectangle 3 5 "solid" "white")
               (rectangle 3 5 "solid" "black")
               )
              (rectangle 12 3 "solid" "black")
              (rectangle 20 5 "solid" "black"))
  )

(define UFO (text "<o>" 15 "black"))

(define WIDTH 300)
(define HEIGHT 200)
(define MID-Y (/ HEIGHT 2))
(define SLUG-SPEED 1) ;; number of pixels per tick that slug tavels up
(define UFO-SPEED 1)  ;; number of pixels per tick that ufo moves
(define BOMB-SPEED 1) ;; number of pixels per tick that bomb falls
(define ACCURACY 5)   ;; number of pixels of the distance between slug and object to hit

(define MTS (empty-scene WIDTH HEIGHT))

;; Pos is (make-pos x y)
;; interp. Pos is position of an element in the screen: x and y are coordinates

(define-struct pos (x y))

(define P0 (make-pos (/ WIDTH 2) (- HEIGHT (/ (image-height TANK) 2))) ) ; starting Tank position
(define P1 (make-pos WIDTH (/ HEIGHT 2))) ; starting UFO position
(define P2 (make-pos (/ WIDTH 2) (/ HEIGHT 2))) ; midscreen
(define P3 (make-pos (/ WIDTH 2) 1)) ; 1 pixel below ceiling
(define DROP-POSITION (make-pos (+ (pos-x P0) 30) (- (pos-y P0) 30)))

#;
(define (fn-for-pos b)
  (... (pos-x b)     ;Natural
       (pos-y b)))   ;Natural
;; Template rules used:
;;  - compound: 2 fields


;; Pos Pos -> Boolean
;; true if the positions differ by less than ACCURACY pixel

(check-expect (collide? P0 P0) true)
(check-expect (collide? P0 P1) false)
(check-expect (collide? P2 (make-pos (+ ACCURACY (pos-x P2)) (+ ACCURACY (pos-y P2)))) true)
(check-expect (collide? P2 (make-pos (+ (+ 1 ACCURACY) (pos-x P2)) (+ ACCURACY (pos-y P2)))) false)
(check-expect (collide? (tank-slug TANK-2) (ufo-pos UFO-1)) true)

; (define (collide? p1 p2) false) ; stub

(define (collide? p1 p2)
  (and
   (<= (abs (- (pos-x p1) (pos-x p2))) ACCURACY)
   (<= (abs (- (pos-y p1) (pos-y p2))) ACCURACY)
   )
  )

;; Pos Pos -> Boolean
;; true if p1 and p2 are connected by a diagonal line with 45 angle degree (1 pixel tolerance)
;;  it is the point the ufo drops bomb but only when ufo flyes right -> left
;;  p1 is ufo, p2 is tank

(check-expect (dia45? P0 P0) true)
(check-expect (dia45? DROP-POSITION P0) true)
(check-expect (dia45? P0 P1) false)
(check-expect (dia45? P0 (make-pos (+ (pos-x P0) 20) (- (pos-y P0) 21))) true)
(check-expect (dia45? P0 (make-pos (+ (pos-x P0) 20) (- (pos-y P0) 22))) false)

;(define (dia45? p1 p2) false) ; stub

(define (dia45? p1 p2)
  (<= (abs (-
            (- (pos-x p1) (pos-x p2))
            (- (pos-y p2) (pos-y p1))
            )) 1)
  )

;; Score is one of:
;; - false
;; - String
;; interp: if false game is not over, otherwise it tells who is the winner

(define SCORE-START "press SPACE to start")
(define UFO-WON "UFO won. Press SPACE to play again")
(define TANK-WON "TANK won. Press SPACE to play again")

#;
(define (fn-for-score s)
  (cond [(false? s) (...)]
        [(... s)]))

;; Score Image -> Image
;; places on background bg a score; if score is false render bg

(check-expect (place-score UFO-WON MTS) (place-image (text UFO-WON 15 "black") (pos-x S2) (pos-y S2) MTS) )
(check-expect (place-score false MTS) MTS)

;;(define (place-score score bg) bg) ; stub

(define (place-score score bg)
  (if (false? score)
      bg
      (place-image (text score 15 "black") (pos-x S2) (pos-y S2) bg)
      )
  )

;; Slug is one of:
;; - false
;; - pos
;; interp. false means not yet shot, otherwise pos it is the Pos

(define S1 false)        ; not yet shot
(define S2 P2)           ; midflight

#;
(define (fn-for-slug s)
  (cond [(false? s) (...)]
        [(pos? s) (fn-for-pos (slug-pos s))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: false
;;  - reference: pos is a Pos


;; Slug -> Slug
;; moves a slug SLUG-SPEED pixel up; if false moves nothing; if reaches ceiling, slug -> false

(check-expect (adv-slug false) false)
(check-expect (adv-slug P2) (make-pos (pos-x P2) (- (pos-y P2) SLUG-SPEED)))
(check-expect (adv-slug P3) false)

;; (define (adv-slug s) false) ;stub
;; template from Slug

(define (adv-slug s)
  (cond [(false? s) false]
        [(<= (- (pos-y s) SLUG-SPEED) 0) false]
        [else (make-pos (pos-x s) (- (pos-y s) SLUG-SPEED))]
        ))

;; Slug Image Image -> Image
;; places on background bg a given image (SLUG or BOMB) at correct pos; if slug is false render bg

(check-expect (place-slug S2 SLUG MTS) (place-image SLUG (pos-x S2) (pos-y S2) MTS) )
(check-expect (place-slug S1 SLUG MTS) MTS)


;;(define (place-slug slug img bg) MTS) ; stub

(define (place-slug s img bg)
  (if (false? s)
      bg
      (place-image img (pos-x s) (pos-y s) bg)
      )
  )

;; Slug -> Slug
;; moves a bomb BOMB-SPEED pixels letf-down; if false moves nothing; if reaches ground, bomb -> false

(check-expect (adv-bomb false) false)
(check-expect (adv-bomb P2) (make-pos (- (pos-x P2) BOMB-SPEED) (+ (pos-y P2) BOMB-SPEED)))
(check-expect (adv-bomb (make-pos (/ WIDTH 2) (- HEIGHT BOMB-SPEED))) false)

;; (define (adv-bomb s) false) ;stub
;; template from Slug

(define (adv-bomb s)
  (cond [(false? s) false]
        [(>= (+ (pos-y s) BOMB-SPEED) HEIGHT) false]
        [else (make-pos (- (pos-x s) UFO-SPEED) (+ (pos-y s) UFO-SPEED))]
        ))

;; Slug Pos -> Boolean
;; true if slug collide? with object in pos. Always false if slug is false

(check-expect (slug-collide? false P0) false)
(check-expect (slug-collide? P0 P0) (collide? P0 P0))

;;(define (slug-collide? s p) false) ;stub

(define (slug-collide? s p)
  (if (false? s)
      false
      (collide? s p)
      )
  )


;; Tank is (make-tank p slug)
;; interp. pos is tank pos; slug is a slug

(define-struct tank (pos slug))

(define TANK-1 (make-tank P0 S1))  ;; tank is half screen, loaded
(define TANK-2 (make-tank P0 S2))  ;; tank is half screen, slug halfway

#;
(define (fn-for-tank t)
  (... (fn-for-pos (tank-pos t))          ;Pos
       (fn-for-slug (tank-slug t))))      ;Slug

;; Template rules used:
;; - compound: 2 fields
;;  - reference: pos field is Pos
;;  - reference: slug field is slug


;; Tank Numeric -> Tank
;; moves tank by x pixel (-1 is one left, +1 is one right)

(check-expect (move-tank TANK-1 1) (make-tank (make-pos (+ 1 (pos-x (tank-pos TANK-1))) (pos-y (tank-pos TANK-1)) ) (tank-slug TANK-1)) )

(define (move-tank t drift)
  (make-tank
   (make-pos (+ drift (pos-x (tank-pos t))) (pos-y (tank-pos t)))
   (tank-slug t)
   )
  )


;; Tank -> Boolean
;; true if slug is inflight, false means not yet fired

(check-expect (fired? TANK-1) false)
(check-expect (fired? TANK-2) true)

;(define (fired? t) false) ; stub
; used template from tank
(define (fired? t)
  (not (false? (tank-slug t)) )
  )

;; Tank -> Image
;; places on background bg the tank image at correct pos

(check-expect (place-tank TANK-1 MTS) (place-image TANK (pos-x (tank-pos TANK-1)) (pos-y (tank-pos TANK-1)) MTS) )

;(define (place-tank tank bg) MTS) ; stub

(define (place-tank t bg)
  (place-image TANK (pos-x (tank-pos t)) (pos-y (tank-pos t)) bg)
  )


;; UFO is (make-ufo pos bomb)
;; interp. p is a pos; bomb is Slug

(define-struct ufo (pos bomb))

(define UFO-1 (make-ufo P2 P0))      ;; ufo is half screen, bomb reaching ground
(define UFO-2 (make-ufo P1 false))   ;; ufo is at start screen, bomb not dropped

#;
(define (fn-for-ufo u)
  (... (ufo-pos u)           ;Pos
       (ufo-bomb u)))   ;Slug

;; Template rules used:
;; - compound: 2 fields
;; - reference: Pos
;; - reference: Slug

;; (define (adv-slug s) false) ;stub
;; template from Slug

;; Ufo -> Ufo
;; moves ufo UFO-SPEED pixel leftwards; if reaches left border, reset to right border


(check-expect (ufo-pos (adv-ufo UFO-1)) (make-pos (- (pos-x P2) UFO-SPEED) (pos-y P2)) )
(check-expect (ufo-pos (adv-ufo (make-ufo (make-pos SLUG-SPEED MID-Y) false))) P1)

;(define (adv-ufo u) UFO-1) ;stub

(define (adv-ufo u)
  (make-ufo (cond 
              [(<= (pos-x (ufo-pos u)) UFO-SPEED) (make-pos WIDTH MID-Y)]
              [else (make-pos (- (pos-x (ufo-pos u)) UFO-SPEED) (pos-y (ufo-pos u)))]
              ) (ufo-bomb u))
  )
;; Ufo -> Image
;; places on background bg the ufo image at correct pos

(check-expect (place-ufo UFO-1 MTS) (place-image UFO (pos-x S2) (pos-y S2) MTS) )


;(define (place-ufo ufo bg) MTS) ; stub

(define (place-ufo u bg)
  (place-image UFO (pos-x (ufo-pos u)) (pos-y (ufo-pos u)) bg)
  )

;; Ufo Tank -> Ufo
;; if bomb is dropped -> Ufo is unchanged else if Ufo pos and Tank pos are at drop
;;  position (45 degrees) Ufo's Bomb is at Ufo's position.


(check-expect (drop-bomb (make-ufo DROP-POSITION false) TANK-1)
              (make-ufo DROP-POSITION DROP-POSITION))
(check-expect (drop-bomb (make-ufo DROP-POSITION P2) TANK-1)
              (make-ufo DROP-POSITION P2))
(check-expect (drop-bomb UFO-2 TANK-1) UFO-2)

;;(define (drop-bomb ufo tank) ufo) ; stub
(define (drop-bomb ufo tank)
  (if (false? (ufo-bomb ufo))
      (if (dia45? (ufo-pos ufo) (tank-pos tank))
          (make-ufo (ufo-pos ufo) (ufo-pos ufo))
          ufo
          )
      ufo
      )
  )

;; World is (make-world tank ufo score)
;; interp. tank is the Tank, ufo the UFO, score the game score

(define-struct world (tank ufo score))

(define WSTART (make-world TANK-1 UFO-2 SCORE-START)) ; world start 
(define W0 (make-world TANK-1 UFO-2 false)) ; space pressed 
(define W1 (make-world TANK-2 UFO-1 false))
(define W2 (make-world TANK-2 UFO-2 false))



;; World -> World
;; start the world with (main WSTART)
;; 
(define (main ws)
  (big-bang ws                     ; World
            (on-tick   flow)       ; World -> World
            (to-draw   render)     ; World -> Image
            (on-key    act-tank))) ; World KeyEvent -> World

;; World -> World
;; produce the next state in the world: the slug moves up ...

(check-expect (pos-y (tank-slug (world-tank (flow W2)))) (pos-y (adv-slug (tank-slug (world-tank W2)))) )

;(define (flow ws) W0) ;stub

(define (flow w)
  (if (slug-collide? (ufo-bomb (world-ufo w)) (tank-pos (world-tank w)))
      (make-world (world-tank w) (world-ufo w) UFO-WON)
      (if (slug-collide? (tank-slug (world-tank w)) (ufo-pos (world-ufo w)))
          (make-world (world-tank w) (world-ufo w) TANK-WON)
          (if (false? (world-score w))
              (make-world (make-tank (tank-pos (world-tank w)) (adv-slug (tank-slug (world-tank w))))
                          (drop-bomb (make-ufo (ufo-pos (adv-ufo (world-ufo w))) (adv-bomb (ufo-bomb (world-ufo w))) )
                                     (world-tank w))
                          (world-score w)
                          )
              w
              )
          )
      )
  )


;; World -> Image
;; render world

(define (render ws)
  (if (false? (world-score ws))
      (place-ufo (world-ufo ws)
                 (place-tank (world-tank ws)
                             (place-slug (ufo-bomb (world-ufo ws)) BOMB               
                                         (place-slug (tank-slug (world-tank ws)) SLUG MTS)               
                                         )
                             )
                 )
      (place-score (world-score ws) MTS)
      )
  )

;; World keyEvent -> World
;; move tank left or right according key pressed; if space pressed fires slug unless already inflight
;; !!!
(check-expect (act-tank W0 "left") (make-world (move-tank (world-tank W0) -1) (world-ufo W0) (world-score W0)))
(check-expect (act-tank W0 "right") (make-world (move-tank (world-tank W0) 1) (world-ufo W0) (world-score W0)))
(check-expect (act-tank W0 " ") (make-world (make-tank (tank-pos (world-tank W0)) (tank-pos (world-tank W0)) ) (world-ufo W0) (world-score W0)))
(check-expect (act-tank W1 " ") W1)
(check-expect (act-tank W0 "a") W0)


;(define (act-tank ke ws) ws) ; stub

(define (act-tank ws ke)
  (cond [(key=? ke " ")
         (if (false? (world-score ws))
             (if (fired? (world-tank ws))
                 ws
                 (make-world (make-tank (tank-pos (world-tank ws)) (tank-pos (world-tank ws))) (world-ufo ws) (world-score ws))
                 )
             W0
             )
         ]
        [(key=? ke "right") (make-world (move-tank (world-tank ws) 1) (world-ufo ws) (world-score ws))]
        [(key=? ke "left") (make-world (move-tank (world-tank ws) -1) (world-ufo ws) (world-score ws))]
        [else ws])
  )



