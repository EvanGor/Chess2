#lang typed/racket
(require typed/test-engine/racket-tests)
(require "uchicago151/uchicago151.rkt")
(require "uchicago151/uc151image.rkt")
(require "uchicago151/uc151universe.rkt")
(require "chess-logic.rkt")
(require "loc.rkt")
(require "option.rkt")


;; Dims is a structure to contain integer dimensions.
(define-struct Dims
  ([width  : Integer]
   [height : Integer]))

(define-struct (Sized T)
  ([t    : T]
   [dims : Dims]))

(define-type (Reporter T)
  (T -> String))

(define-type CellColor
  (U "ivory" "lightgray" "darkgray"))

(define-struct Cell
  ([color : CellColor]))

(define-struct Promote ([pawnstart : Loc]
                        [pawndest : Loc]))
(define-type Mode (U 'Start Promote (Pr Piece Loc))) 
;; 'start means no piece has been selected to move.
;; If mode is of form (Pr Piece Loc), then piece has been selected to move.
;; Promote means player must select piece to promote pawn to.

(define-struct ChessWorld
  ([game : (Sized ChessGame)]
   [reportsize : Dims]
   [mode : Mode]))

;; We define chessgame values to be used for testing.
(define cg1 (ChessGame
 (list
  (list 'None (Some (Piece 'Queen 'White))
        'None 'None 'None 'None 'None (Some (Piece 'King 'White)))
  (list 'None 'None 'None 'None 'None 'None 'None 'None)
  (list 'None 'None 'None (Some (Piece 'Pawn 'White)) 'None 'None 'None 'None)
  (list 'None 'None 'None 'None 'None 'None 'None 'None)
  (list 'None 'None 'None 'None 'None 'None 'None 'None)
  (list 'None 'None 'None (Some (Piece 'Rook 'Black)) 'None 'None 'None 'None)
  (list 'None 'None 'None 'None 'None 'None 'None 'None)
  (list 'None 'None 'None 'None 'None 'None 'None (Some (Piece 'King 'Black))))
 'Black
 (list
  (StdMove (Loc 3 4) (Loc 2 3) (Piece 'Pawn 'White) (Some (Piece 'Pawn 'Black)))
  (StdMove (Loc 1 3) (Loc 3 3) (Piece 'Pawn 'Black) 'None))
 (Castles #t #t #t #t)))

(define cg2
  (ChessGame
   (list
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list 'None 'None 'None
          (Some (Piece 'Pawn 'White)) 'None 'None 'None
          (Some (Piece 'Rook 'White)))
    '(None None None None None None None None)
    '(None None None None None None None None)
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list 'None 'None 'None 'None 'None 'None 'None
          (Some (Piece 'King 'Black))))
   'Black
   '()
   (Castles #t #t #t #t)))

(define cg3
  (ChessGame
   (list
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list 'None 'None 'None
          (Some (Piece 'Pawn 'White)) 'None 'None (Some (Piece 'Queen 'White))
          (Some (Piece 'Rook 'White)))
    '(None None None None None None None None)
    '(None None None None None None None None)
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list 'None 'None 'None 'None 'None 'None 'None
          (Some (Piece 'King 'Black))))
   'Black
   '()
   (Castles #t #t #t #t)))

(define cg4
  (ChessGame
   (list
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list 'None 'None 'None
          (Some (Piece 'Pawn 'White)) 'None 'None (Some (Piece 'Queen 'White))
          (Some (Piece 'Rook 'White)))
    '(None None None None None None None None)
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list (Some (Piece 'Pawn 'Black)) 'None 'None 'None 'None 'None 'None 'None)
    (list 'None 'None 'None 'None 'None 'None 'None
          (Some (Piece 'Queen 'Black))))
   'Black
   '()
   (Castles #t #t #t #t)))

(define cg5
  (ChessGame
   (list
    (list (Some (Piece 'Rook 'Black)) 'None 'None
          (Some (Piece 'King 'Black)) 'None 'None 'None 'None)
    '(None None None None None None None None)
    (list 'None 'None 'None
          (Some (Piece 'Pawn 'White)) 'None 'None 'None
          (Some (Piece 'Rook 'White)))
    '(None None None None None None None None)
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list (Some (Piece 'Pawn 'Black)) 'None 'None 'None 'None 'None 'None 'None)
    (list 'None 'None 'None 'None 'None 'None 'None
          (Some (Piece 'King 'Black))))
   'Black
   '()
   (Castles #t #t #t #t)))
             
;; We also define ChessWorld values to be used in testing.            
(define cw1 (ChessWorld (Sized cg1 (Dims 400 200)) (Dims 50 100)
                        (Pr (Piece 'Rook 'Black) (Loc 5 3))))
(define cw2 (ChessWorld (Sized cg2 (Dims 300 300)) (Dims 50 50)
                        (Pr (Piece 'King 'Black) (Loc 7 7))))
(define cw3 (ChessWorld (Sized cg3 (Dims 300 100)) (Dims 75 75) 'Start))
(define cw4 (ChessWorld (Sized cg4 (Dims 100 100)) (Dims 50 90)
                        (Promote (Loc 6 0) (Loc 7 0))))
(define cw5 (ChessWorld (Sized cg5 (Dims 300 300)) (Dims 80 80)
                        (Pr (Piece 'King 'Black) (Loc 0 3))))


(: notturn (ChessGame -> Player))
;; Returns the player whose turn it isn't.

(define (notturn cg)
  (if (symbol=? (ChessGame-turn cg) 'White) 'Black 'White))

(check-expect (notturn new-game) 'Black)
(check-expect (notturn (ChessGame starting-board 'Black '() (Castles #t #f #t #t)))
              'White)



(: new-chess-world (ChessGame Dims Dims -> ChessWorld))
;; Builds interface from chess game (with mode 'start).

(define (new-chess-world cg d1 d2)
  (ChessWorld (Sized cg d1) d2 'Start))

(check-expect (new-chess-world new-game (Dims 400 200) (Dims 300 300))
              (ChessWorld (Sized new-game (Dims 400 200)) (Dims 300 300) 'Start))



(: report (Reporter ChessWorld))
;; If game is in progress, returns string with
;; information on whose turn it is, if this player is in check, and,
;; if pawn promotion, taking place, instructions on how to choose
;; piece to replace pawn.
;; If game has ended, indicates winner or that stalemate has occurred.

(define (report cw)
  (local {(define cg (Sized-t (ChessWorld-game cw)))}
    (cond
      [(checkmate? cg)
       (string-append "Checkmate: "
                      (symbol->string (notturn cg))
                      " wins.")]
      [(stalemate? cg)
       "Stalemate: Draw."]
      [else
       (string-append
        (symbol->string (ChessGame-turn cg))
        (if (in-check? cg)
            " in check:"
            "'s turn:")
        (match (ChessWorld-mode cw)
          [(Pr _ _) " Choose destination."]
          [(Promote _ _) " Type r to promote to rook. Type b to promote to bishop. Type k to promote to knight. Type q to promote to queen."]
;; No way to keep the above line less that 80 characters because it includes a long string.
          ['Start " Choose piece to move."]))])))

(check-expect (report (ChessWorld (Sized cg1 (Dims 200 300)) (Dims 400 100) (Pr (Piece 'King 'Black) (Loc 7 7))))
              "Black's turn: Choose destination.")
         
(check-expect (report (new-chess-world new-game (Dims 5 5) (Dims 10 10)))
              "White's turn: Choose piece to move.")

(check-expect (report cw2) "Black in check: Choose destination.")
(check-expect (report cw3) "Checkmate: White wins.")
(check-expect (report cw4) "Black's turn: Type r to promote to rook. Type b to promote to bishop. Type k to promote to knight. Type q to promote to queen.") 


(: guarantee-byte (Integer -> Byte))
(define (guarantee-byte n)
  (if (byte? n) n (error "not a byte")))

(: draw-chess-row (Boolean Mode (Listof Square) Integer Integer -> Image))
;; Draws row of chess board with given width and height in pixels.

(define (draw-chess-row bool m ss l h)
    (match ss
          ['() empty-image]
          [(cons a rs)
           (beside
            (match a
              ['None
               (overlay
                (rectangle (quotient l (length ss)) h "outline" "black") 
                (rectangle (quotient l (length ss)) h "solid"
                           (if bool
                           (if (even? (length ss)) "beige" "brown")
                           (if (even? (length ss)) "brown" "beige"))))]
              [(Some (Piece tp col))
               (overlay
                (text
                 (cond
                   [(symbol=? tp 'King) (if (symbol=? col 'White) "♔" "♚")]
                   [(symbol=? tp 'Queen) (if (symbol=? col 'White) "♕" "♛")]
                   [(symbol=? tp 'Rook) (if (symbol=? col 'White) "♖" "♜")]
                   [(symbol=? tp 'Bishop) (if (symbol=? col 'White) "♗" "♝")]
                   [(symbol=? tp 'Knight) (if (symbol=? col 'White) "♘" "♞")]
                   [(symbol=? tp 'Pawn) (if (symbol=? col 'White) "♙" "♟")])
                 (guarantee-byte (add1 (min (round (* (/ 11 20)
                                                      (/ l (length ss))))
                                            (round (* (/ 11 20) h)))))
                 "black")                       
                (rectangle (quotient l (length ss)) h "outline" "black")
                (rectangle
                 (quotient l (length ss)) h "solid"
                 (if
                  (symbol? m)
                  (if bool
                   (if (even? (length ss)) "beige" "brown")
                   (if (even? (length ss)) "brown" "beige"))
                  (if 
                    (= (- 8 (length ss)) (Loc-col (if (Pr? m)
                                                      (Pr-second m)
                                                      (Promote-pawnstart m))))
                    "yellow"
                    (if bool
                        (if (even? (length ss)) "beige" "brown")
                        (if (even? (length ss)) "brown" "beige"))))))])
                    (draw-chess-row bool m rs (- l (quotient l (length ss))) h))]))
        
        
        
  
(: draw-chess-board (Mode Board Integer Integer -> Image))  
;; Draws chess board with given width and height in pixels.

(define (draw-chess-board m b l h)
  (match b
    ['() empty-image]
    [(cons a rs)
         (above (draw-chess-row
                 (if (even? (length b)) #t #f)
                 (if (or
                      (and (Pr? m)
                           (= (Loc-row (Pr-second m))
                              (- 8 (length b))))
                      (and (Promote? m)
                           (= (Loc-row (Promote-pawnstart m))
                              (- 8 (length b)))))
                     m 'Start)
                 a l (quotient h (length b)))
                (draw-chess-board m rs l (- h (quotient h (length b)))))]))
        
 

(: draw-chess-world (ChessWorld -> Image))
;; Draws chess world.

(define (draw-chess-world cw)
  (match cw
    [(ChessWorld (Sized cg (Dims bw bh)) (Dims rw rh) m)
     (beside/align "top" (draw-chess-board m (ChessGame-board cg) bw bh)
                   (overlay (text (report cw)
                                  (if (Promote? m)
                                      (guarantee-byte
                                       (add1 (min rh
                                                  (round (* (/ 8 500) rw)))))
                                      (guarantee-byte
                                       (add1 (min rh 
                                                  (round (* (/ 18 500) rw))))))
                                  "black")
                            (rectangle rw rh "solid" "gray")))]))

    
(: handle-click (ChessWorld Integer Integer Mouse-Event -> ChessWorld))
;; Advances the ChessWorld in response to mouse clicks.  Allows for the
;; progression of the game of chess, and, in particular, piece movement.
  

(define (handle-click cw x y me)
  (if (not (string=? me "button-down")) cw
      (local {(define Dimsb (Sized-dims (ChessWorld-game cw)))}
           (if (and (< y (Dims-height Dimsb))
                    (< x (Dims-width Dimsb))
                    (not (checkmate? (Sized-t (ChessWorld-game cw))))
                    (not (stalemate? (Sized-t (ChessWorld-game cw)))))
               (local {(define Dimsr (ChessWorld-reportsize cw))
                       (define r# (floor (* (/ y (Dims-height Dimsb)) 8)))
                       (define c# (floor (* (/ x (Dims-width Dimsb)) 8)))
                       (define debug
                         (begin (display "loc1 is ")
                                (display (Loc r# c#))))
                       (define cg (Sized-t (ChessWorld-game cw)))
                       (define m (ChessWorld-mode cw))
                       (define turn (ChessGame-turn cg))
                       (define sq (board-ref (ChessGame-board cg) (Loc r# c#)))}
                 (match m
                   ['Start
                    (match sq
                      ['None cw]
                      [(Some (Piece tp col))
                       (if (symbol=? turn col)
                           (ChessWorld
                            (Sized cg Dimsb)
                            Dimsr 
                            (Pr (Piece tp col)
                                (Loc r# c#))) cw)])]
                   [(Pr (Piece selpiece c) selloc)
                    (if (and (= (Loc-row selloc) r#)
                             (= (Loc-col selloc) c#))
                        (ChessWorld (Sized cg Dimsb) Dimsr 'Start)
                        (cond
                          [(and (symbol=? 'King selpiece)
                                (or
                                 (and
                                  (= (Loc-row selloc) 0)
                                  (= (Loc-col selloc) 4)
                                  (= r# 0)
                                  (or
                                   (= c# 2)
                                   (= c# 6)))
                                 (and
                                  (= (Loc-row selloc) 7)
                                  (= (Loc-col selloc) 4)
                                  (= r# 7)
                                  (or
                                   (= c# 2)
                                   (= c# 6)))))
                           (if (legal-move? cg (CastleMove
                                                selloc
                                                (Loc r# c#)
                                                (Loc r# (if (= c# 2) 0 7))
                                                (Loc r# (if (= c# 2) 3 5))
                                                turn))
                               (ChessWorld
                                (Sized
                                 (apply-move
                                  cg
                                  (CastleMove
                                   selloc
                                   (Loc r# c#)
                                   (Loc r# (if (= c# 2) 0 7))
                                   (Loc r# (if (= c# 2) 3 5))
                                   turn)) Dimsb) Dimsr 'Start) cw)]
                          [(and (symbol=? 'Pawn selpiece)
                                (or (= r# 0) (= r# 7)))
                           (if (legal-move? cg (PromoMove
                                                selloc
                                                (Loc r# c#)
                                                (Piece 'Pawn turn)
                                                sq
                                                'Queen))
                               (ChessWorld
                                (Sized cg Dimsb) Dimsr
                                (Promote selloc
                                         (Loc r# c#)))
                               cw)]
                          [(and (symbol=? selpiece 'Pawn)
                                (symbol? sq)
                                (or
                                 (= (Loc-row selloc) 3)
                                 (= (Loc-row selloc) 4))
                                (= (abs (- (Loc-row selloc) r#)) 1)
                                (= (abs (- (Loc-col selloc) c#)) 1))
                           (if (legal-move? cg
                                            (StdMove
                                             selloc
                                             (Loc r# c#)
                                             (Piece 'Pawn turn)
                                             (board-ref
                                              (ChessGame-board cg)
                                              (Loc (if
                                                    (= (Loc-row selloc)
                                                       3) 3 4) c#))))
                               (ChessWorld
                                (Sized
                                 (apply-move
                                  cg 
                                  (StdMove
                                   selloc
                                   (Loc r# c#)
                                   (Piece 'Pawn turn)
                                   (board-ref
                                    (ChessGame-board cg)
                                    (Loc (if (= (Loc-row selloc) 3) 3 4) c#))))
                                 Dimsb)
                                Dimsr 'Start) cw)]                               
                          [else (if (legal-move? cg (StdMove
                                                     selloc
                                                     (Loc r# c#)
                                                     (Piece selpiece c)
                                                     sq))
                                    (ChessWorld
                                     (Sized
                                      (apply-move
                                       cg
                                       (StdMove
                                        selloc
                                        (Loc r# c#)
                                        (Piece selpiece c)
                                        sq)) Dimsb) Dimsr 'Start) cw)]))]
                   [(Promote _ _) cw])) cw))))
          

(check-expect (handle-click cw2 260 295 "button-down")
              (ChessWorld (Sized (ChessGame
   (list
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list 'None 'None 'None
          (Some (Piece 'Pawn 'White)) 'None 'None 'None
          (Some (Piece 'Rook 'White)))
    '(None None None None None None None None)
    '(None None None None None None None None)
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list 'None 'None 'None 'None 'None 'None
          (Some (Piece 'King 'Black)) 'None))
   'White
   (list (StdMove (Loc 7 7) (Loc 7 6) (Piece 'King 'Black) 'None))
   (Castles #t #t #t #f)) (Dims 300 300)) (Dims 50 50) 'Start))

(check-expect (handle-click cw2 260 260 "button-down")
              (ChessWorld (Sized (ChessGame
   (list
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list 'None 'None 'None
          (Some (Piece 'Pawn 'White)) 'None 'None 'None
          (Some (Piece 'Rook 'White)))
    '(None None None None None None None None)
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list 'None 'None 'None 'None 'None 'None (Some (Piece 'King 'Black)) 'None)
    (list 'None 'None 'None 'None 'None 'None
          'None 'None))
   'White
   (list (StdMove (Loc 7 7) (Loc 6 6) (Piece 'King 'Black) 'None))
   (Castles #t #t #t #f)) (Dims 300 300)) (Dims 50 50) 'Start))

(check-expect (handle-click cw2 290 290 "button-down")
              (ChessWorld (Sized (ChessGame
   (list
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list 'None 'None 'None
          (Some (Piece 'Pawn 'White)) 'None 'None 'None
          (Some (Piece 'Rook 'White)))
    '(None None None None None None None None)
    '(None None None None None None None None)
    '(None None None None None None None None)
    '(None None None None None None None None)
    (list 'None 'None 'None 'None 'None 'None
          'None (Some (Piece 'King 'Black))))
   'Black
   '()
   (Castles #t #t #t #t)) (Dims 300 300)) (Dims 50 50) 'Start))



(: handle-key (ChessWorld String -> ChessWorld))
;; Facilitates pawn promotion.

(define (handle-key cw s)
  (if (Promote? (ChessWorld-mode cw))
      (local {(define Dimsb (Sized-dims (ChessWorld-game cw)))
              (define Dimsr (ChessWorld-reportsize cw))
              (define cg (Sized-t (ChessWorld-game cw)))
              (define turn (ChessGame-turn cg))
              (define st (Promote-pawnstart (ChessWorld-mode cw)))
              (define end (Promote-pawndest (ChessWorld-mode cw)))
              (define b (ChessGame-board
                         (Sized-t (ChessWorld-game cw))))}
        (match s
          ["q" (ChessWorld
                (Sized
                 (apply-move cg
                             (PromoMove
                              st
                              end
                              (Piece 'Pawn turn)
                              (board-ref b end)
                              'Queen)) Dimsb)                 
                Dimsr
                'Start)]
          ["r" (ChessWorld
                (Sized
                 (apply-move cg
                             (PromoMove
                              st
                              end
                              (Piece 'Pawn turn)
                              (board-ref b end)
                              'Rook))  Dimsb) 
                Dimsr
                'Start)]
          ["b" (ChessWorld
                (Sized
                 (apply-move cg
                             (PromoMove
                              st
                              end
                              (Piece 'Pawn turn)
                              (board-ref b end)
                              'Bishop)) Dimsb)                 
                Dimsr
                'Start)]
          ["k" (ChessWorld
                (Sized
                 (apply-move cg
                             (PromoMove
                              st
                              end
                              (Piece 'Pawn turn)
                              (board-ref b end)
                              'Knight)) Dimsb)                 
                Dimsr
                'Start)]
          [_ cw])) cw))


(check-expect
 (handle-key cw4 "q")
 (ChessWorld
  (Sized (ChessGame
          (list
           '(None None None None None None None None)
           '(None None None None None None None None)
           (list 'None 'None 'None
                 (Some (Piece 'Pawn 'White)) 'None 'None
                 (Some (Piece 'Queen 'White))
                 (Some (Piece 'Rook 'White)))
           '(None None None None None None None None)
           '(None None None None None None None None)
           '(None None None None None None None None)
           (list 'None 'None 'None 'None
                 'None 'None 'None 'None)
           (list (Some (Piece 'Queen 'Black)) 'None 'None 'None 'None 'None
                 'None (Some (Piece 'Queen 'Black))))
          'White
          (list (PromoMove (Loc 6 0) (Loc 7 0) (Piece 'Pawn 'Black)
                           'None 'Queen))
          (Castles #t #t #t #t)) (Dims 100 100))
 (Dims 50 90)
 'Start))

(check-expect
 (handle-key cw4 "k")
 (ChessWorld
  (Sized (ChessGame
          (list
           '(None None None None None None None None)
           '(None None None None None None None None)
           (list 'None 'None 'None
                 (Some (Piece 'Pawn 'White)) 'None 'None
                 (Some (Piece 'Queen 'White))
                 (Some (Piece 'Rook 'White)))
           '(None None None None None None None None)
           '(None None None None None None None None)
           '(None None None None None None None None)
           (list 'None 'None 'None 'None
                 'None 'None 'None 'None)
           (list (Some (Piece 'Knight 'Black)) 'None 'None 'None 'None 'None
                 'None (Some (Piece 'Queen 'Black))))
          'White
          (list (PromoMove (Loc 6 0) (Loc 7 0) (Piece 'Pawn 'Black)
                           'None 'Knight))
          (Castles #t #t #t #t)) (Dims 100 100))
 (Dims 50 90)
 'Start))


(: main (ChessGame Dims Dims -> ChessWorld))                                         
;;Runs chess game gui.      

(define (main cg dimsb dimsr)
  (big-bang
   (new-chess-world cg dimsb dimsr) : ChessWorld
   [to-draw draw-chess-world]
   [on-mouse handle-click]
   [on-key handle-key]
   [name "Chess Game"]))


(main new-game (Dims 600 500) (Dims 800 200))

(test)



