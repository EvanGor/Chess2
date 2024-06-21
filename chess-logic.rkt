#lang typed/racket
(require "uchicago151/uchicago151.rkt")
(require "uchicago151/uc151image.rkt")
(require typed/test-engine/racket-tests)
(require "option.rkt")
(require "loc.rkt")

(define-type PieceType (U 'Pawn 'Bishop 'Knight 'Rook 'King 'Queen))
(define-type Player (U 'Black 'White))
(define-struct Piece
  ([type  : PieceType]
   [color : Player]))
(provide PieceType Player (struct-out Piece))

(define-type Square (Option Piece))
(provide Square)

(define-type Board (Listof (Listof Square)))
(provide Board)

(define-struct StdMove
  ([src : Loc]
   [dst : Loc]
   [moved : Piece]
   [captured : (Option Piece)]))
(provide (struct-out StdMove))

(define-struct CastleMove
  ([king-src : Loc]
   [king-dst : Loc]
   [rook-src : Loc]
   [rook-dst : Loc]
   [moved : Player]))
(provide (struct-out CastleMove))

(define-type PromoteTo (U 'Queen 'Rook 'Bishop 'Knight))
(define-struct PromoMove
  ([src : Loc]
   [dst : Loc]
   [moved : Piece]
   [captured : (Option Piece)]
   [promote-to : PromoteTo]))
(provide PromoteTo (struct-out PromoMove))

(define-type Move (U StdMove CastleMove PromoMove))
(provide Move)

(define-struct Castles
  ([black-toward-0-0 : Boolean]
   [black-toward-0-7 : Boolean]
   [white-toward-7-0 : Boolean]
   [white-toward-7-7 : Boolean]))
(provide (struct-out Castles))

(define-struct ChessGame
  ([board : Board]
   [turn : Player]
   [history : (Listof Move)]
   [cas : Castles]))
(provide (struct-out ChessGame))

(define-struct (Pr A B)
  ([first : A]
   [second : B]))
(provide (struct-out Pr))

;; We define values to represent rows in a starting chess board:
(define blackbaserow (list
                      (Some (Piece 'Rook 'Black))
                      (Some (Piece 'Knight 'Black))
                      (Some (Piece 'Bishop 'Black))
                      (Some (Piece 'Queen 'Black))
                      (Some (Piece 'King 'Black))
                      (Some (Piece 'Bishop 'Black))
                      (Some (Piece 'Knight 'Black))
                      (Some (Piece 'Rook 'Black))))

(define blackpawnrow (make-list 8 (Some (Piece 'Pawn 'Black))))

(define emptyrow (list 'None 'None 'None 'None
                      'None 'None 'None 'None))

(: colorswitch (Square -> Square))
;; Toggles the color of the piece if the square contains one.

(define (colorswitch sq)
  (match sq
    ['None 'None]
    [(Some (Piece t col)) (if (symbol=? 'Black col)
                              (Some (Piece t 'White))
                              (Some (Piece t 'Black)))])) 

(check-expect (colorswitch (Some (Piece 'Pawn 'White)))
              (Some (Piece 'Pawn 'Black)))
(check-expect (colorswitch (Some (Piece 'King 'Black)))
              (Some (Piece 'King 'White)))
              

(: starting-board : Board)
;; This value represents the starting layout of a chess board,
;; with white at the bottom.

(define starting-board
  (list blackbaserow blackpawnrow
        emptyrow emptyrow
        emptyrow emptyrow
        (map colorswitch blackpawnrow)
        (map colorswitch blackbaserow)))
(provide starting-board)

(: new-game : ChessGame)
;; Value to represent a new game of chess.

(define new-game
  (ChessGame
   starting-board
   'White
   '()
   (Castles #t #t #t #t)))
(provide new-game)

(: board-ref (Board Loc -> Square))
;; Returns the contents of the specified square in the specified board.

(define (board-ref b l)
  (list-ref (list-ref b (Loc-row l)) (Loc-col l)))

(check-expect (board-ref starting-board (Loc 7 5))
              (Some (Piece 'Bishop 'White)))
(check-expect (board-ref starting-board (Loc 1 2))
              (Some (Piece 'Pawn 'Black)))
(provide board-ref)

(: list-update (All(T) ((Listof T) Integer T -> (Listof T))))
;; Updates the nth item of the list to something new.
;; Raises an error if index out of bounds.
 
(define (list-update xs n x)
  (cond
    [(negative? n) (error "negative index")]
    [(empty? xs) (error "index too high")]
    [(zero? n) (cons x (rest xs))]
    [else (cons (first xs) (list-update (rest xs) (sub1 n) x))]))

(check-expect (list-update (list 1 2 3 4 5) 3 51) (list 1 2 3 51 5))  
(check-expect
 (list-update (list 'a 'v 'd 'e) 0 "hello") (list "hello" 'v 'd 'e))


(: board-update (Board Loc Square -> Board))
;; Returns a board with the contents of the specified location updated.

(define (board-update b l s)
  (list-update b (Loc-row l) (list-update
                              (list-ref b (Loc-row l)) (Loc-col l) s)))
              

(check-expect (board-update starting-board
                            (Loc 2 4)
                            (Some (Piece 'Queen 'White)))
              (list blackbaserow blackpawnrow
                    (list-update emptyrow 4 (Some (Piece 'Queen 'White)))
                    emptyrow emptyrow emptyrow
                    (map colorswitch blackpawnrow)
                    (map colorswitch blackbaserow)))
(check-expect (board-update starting-board
                            (Loc 7 2)
                            (Some (Piece 'King 'Black)))
              (list blackbaserow blackpawnrow
                    emptyrow emptyrow
                    emptyrow emptyrow
                    (map colorswitch blackpawnrow)
                    (list-update
                     (map colorswitch blackbaserow) 2
                     (Some (Piece 'King 'Black)))))

                                            
(: firstpass (All (A) ((A -> Boolean) (Listof A) -> (Option A))))
;; Returns the first item to pass the test, if there is such an item.

(define (firstpass test as)
  (match as
    ['() 'None]
    [(cons a rs) (if (test a) (Some a) (firstpass test rs))]))

(check-expect (firstpass even? (list 3 5 9 17 64 2 5 2)) (Some 64))
(check-expect (firstpass (lambda ([st : String]) (string-ci>? "marry" st))
                    (list "play" "zeal" "x-ray" "rub" "llama" "sad"))
              (Some "llama"))
(check-expect (firstpass zero? (list 1 4 -3 5 9)) 'None)
                                                      
(: rowpair (Loc Board -> (Listof (Pr Loc Square))))
;; Returns the row of the given location
;; with each entry including info on both the location and its contents.

(define (rowpair l b)
  (build-list 8 (lambda ([n : Natural])
                  (Pr (Loc (Loc-row l) n)
                      (board-ref b (Loc (Loc-row l) n))))))

(: colpair (Loc Board -> (Listof (Pr Loc Square))))
;; Returns the column of the given location
;; with each entry including info on both the location and its contents.

(define (colpair l b)
  (build-list 8 (lambda ([n : Natural])
                  (Pr (Loc n (Loc-col l)) (board-ref b (Loc n (Loc-col l)))))))


(: /pair (Loc Board -> (Listof (Pr Loc Square))))
;; Returns a list of elements (Pr Loc Square) corresponding to the diagonal
;; of the given location from southwest to northwest.

(define (/pair l b)
  (local {(define sum (+ (Loc-row l) (Loc-col l)))}
    (if (>= sum 7)
        (build-list (- 15 sum)
                    (lambda ([n : Natural])
                      (Pr (Loc (- 7 n) (+ (- sum 7) n))
                          (board-ref b (Loc (- 7 n) (+ (- sum 7) n))))))
        (build-list (add1 sum)
                    (lambda ([n : Natural])
                      (Pr (Loc (- sum n) n)
                          (board-ref b (Loc (- sum n) n))))))))


(: \pair (Loc Board -> (Listof (Pr Loc Square))))
;; Returns a list of elements (Pr Loc Square) corresponding to the diagonal
;; of the given location from northwest to southeast.

(define (\pair l b)
 (local {(define dif (- (Loc-col l) (Loc-row l)))}
   (if (>= dif 0)
       (build-list (- 8 dif)
                   (lambda ([n : Natural])
                     (Pr (Loc n (+ dif n))
                         (board-ref b (Loc n (+ dif n))))))
       (build-list (+ 8 dif)
                   (lambda ([n : Natural])
                     (Pr (Loc (+ (- dif) n) n)
                         (board-ref b (Loc (+ (- dif) n) n))))))))
                   
                         
(: stoppers ((U 'Bishop 'Rook) Loc Board -> (Listof (Option (Pr Loc Piece)))))
;; Returns in a list the occupied square (if there is one)
;; nearest to the given location in each direction.
;; If input 'Rook, directions are cardinal: (right, below, left, above)
;; If input 'Bishop, directions are diagonal: (NE, SE, SW, NW)
;; The occupied square is given as a (Pr Loc Piece)

(define (stoppers p l b)
  (list
   (match (firstpass
           (lambda ([x : (Pr Loc Square)])
             (match (Pr-second x)
               ['None #f]
               [(Some (Piece t c)) 
                (> (Loc-col (Pr-first x)) (Loc-col l))]))
           (if (symbol=? 'Rook p) (rowpair l b) (/pair l b))) 
     ['None 'None]
     [(Some (Pr lc 'None)) 'None]
     [(Some (Pr lc (Some (Piece tp cl)))) (Some (Pr lc (Piece tp cl)))]) 
   (match (firstpass
           (lambda ([x : (Pr Loc Square)])
             (match (Pr-second x)
               ['None #f]
               [(Some (Piece t c)) 
                (> (Loc-row (Pr-first x)) (Loc-row l))]))
           (if (symbol=? 'Rook p) (colpair l b) (\pair l b)))
     ['None 'None]
     [(Some (Pr lc 'None)) 'None]
     [(Some (Pr lc (Some (Piece tp cl)))) (Some (Pr lc (Piece tp cl)))])
   (match (firstpass
           (lambda ([x : (Pr Loc Square)])
             (match (Pr-second x)
               ['None #f]
               [(Some (Piece t c)) 
                (< (Loc-col (Pr-first x)) (Loc-col l))]))
           (if (symbol=? 'Rook p) (reverse (rowpair l b)) (reverse (/pair l b))))
     ['None 'None]
     [(Some (Pr lc 'None)) 'None]
     [(Some (Pr lc (Some (Piece tp cl)))) (Some (Pr lc (Piece tp cl)))])
   (match (firstpass
           (lambda ([x : (Pr Loc Square)])
             (match (Pr-second x)
               ['None #f]
               [(Some (Piece t c)) 
                (< (Loc-row (Pr-first x)) (Loc-row l))]))
           (if (symbol=? 'Rook p) (reverse (colpair l b)) (reverse (\pair l b))))
     ['None 'None]
     [(Some (Pr lc 'None)) 'None]
     [(Some (Pr lc (Some (Piece tp cl)))) (Some (Pr lc (Piece tp cl)))])))

(check-expect (stoppers 'Rook (Loc 1 3) starting-board)
              (list (Some (Pr (Loc 1 4) (Piece 'Pawn 'Black)))
                    (Some (Pr (Loc 6 3) (Piece 'Pawn 'White)))
                    (Some (Pr (Loc 1 2) (Piece 'Pawn 'Black)))
                    (Some (Pr (Loc 0 3) (Piece 'Queen 'Black)))))
(check-expect (stoppers 'Rook (Loc 7 7) starting-board)
              (list 'None 'None
                    (Some (Pr (Loc 7 6) (Piece 'Knight 'White)))
                    (Some (Pr (Loc 6 7) (Piece 'Pawn 'White)))))

(check-expect (stoppers 'Bishop (Loc 2 6) starting-board)
              (list (Some (Pr (Loc 1 7) (Piece 'Pawn 'Black)))
                    'None
                    (Some (Pr (Loc 6 2) (Piece 'Pawn 'White)))
                    (Some (Pr (Loc 1 5) (Piece 'Pawn 'Black)))))


(: opt-map (All (T U) ((T -> U) (Option T) U -> U)))
;; apply f to value if there is one, otherwise return default value
;; ex: (opt-map add1 'None 0)    => 0
;; ex: (opt-map add1 (Some 4) 0) => 5

(define (opt-map f opt def)
  (match opt
    ['None def]
    [(Some x) (f x)]))

(: move->str (Move -> String))
;; build a string version of the move, for purposes of comparison
;; note: there is a bijection between moves and strings (and must be)

(define (move->str m)
  (match m
    [(StdMove src dst moved captured)
     (pipes (list "StdMove"
                  (loc->str src)
                  (loc->str dst)
                  (piece->str moved)
                  (opt-map piece->str captured "None")))]
    [(CastleMove ks kd rs rd moved)
     (pipes (list "CastleMove"
                  (loc->str ks)
                  (loc->str kd)
                  (loc->str rs)
                  (loc->str rd)
                  (symbol->string moved)))]
    [(PromoMove src dst moved captured pro)
     (pipes (list "PromoMove"
                  (loc->str src)
                  (loc->str dst)
                  (piece->str moved)
                  (opt-map piece->str captured "None")
                  (symbol->string pro)))]))

(: loc->str (Loc -> String))
;; return string representation of location

(define (loc->str loc)
  (match loc
    [(Loc r c)
     (string-append "Loc:" (number->string r) "," (number->string c))]))

(: piece->str (Piece -> String))
;; return string representation of piece

(define (piece->str p)
  (match p
    [(Piece t pl)
     (string-append "Piece:"
                    (symbol->string t)
                    ","
                    (symbol->string pl))]))

(: pipes ((Listof String) -> String))
;; connect strings with | character in between
;; ex: (pipes (list "a" "bb" "ccc")) ==> "a|bb|ccc"

(define (pipes ss)
  (match ss
    ['() ""]
    [(list s) s]
    [(cons s r) (string-append s "|" (pipes r))]))

(: move<? (Move Move -> Boolean))
;; move comparison for the purposes of sorting

(define (move<? m1 m2)
  (string<? (move->str m1) (move->str m2)))

(: sort-moves : (Listof Move) -> (Listof Move))
;; sort a list of moves into a canonical order
;; allowing for comparison with check-expect
;; note: uses the built-in sort operation

(define (sort-moves moves)
  (sort moves move<?))


(: possible-moves-rook (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical rook in the given location
;; can make for the player whose turn it is
;; without considering whether the move puts the player in check.

(define (possible-moves-rook cg l)
  (append
   (match (first (stoppers 'Rook l (ChessGame-board cg)))
     ['None (build-list (- 7 (Loc-col l))
                        (lambda ([k : Natural])
                          (StdMove l
                                   (Loc (Loc-row l) (+ (Loc-col l) (add1 k)))
                                   (Piece 'Rook (ChessGame-turn cg))
                                   'None)))]
     [(Some (Pr lc (Piece tp col)))
      (local {(define bl1
                (build-list
                 (- (sub1 (Loc-col lc))
                    (Loc-col l))
                 (lambda ([k : Natural])
                   (StdMove l
                            (Loc (Loc-row l) (+ (Loc-col l) (add1 k)))
                            (Piece 'Rook (ChessGame-turn cg))
                            'None))))}
        (if (symbol=? (ChessGame-turn cg) col) bl1
            (append bl1
                    (list (StdMove l
                                   (Loc (Loc-row l) (Loc-col lc))
                                   (Piece 'Rook (ChessGame-turn cg))
                                   (Some (Piece tp col)))))))])
   (match (second (stoppers 'Rook l (ChessGame-board cg)))
     ['None (build-list (- 7 (Loc-row l))
                        (lambda ([k : Natural])
                          (StdMove l
                                   (Loc (+ (Loc-row l) (add1 k)) (Loc-col l))
                                   (Piece 'Rook (ChessGame-turn cg))
                                   'None)))]
     [(Some (Pr lc (Piece tp col)))
      (local {(define bl1
                (build-list
                 (- (sub1 (Loc-row lc))
                    (Loc-row l))
                 (lambda ([k : Natural])
                   (StdMove l
                            (Loc (+ (add1 k) (Loc-row l)) (Loc-col l))
                            (Piece 'Rook (ChessGame-turn cg))
                            'None))))}
        (if (symbol=? (ChessGame-turn cg) col) bl1
            (append bl1
                    (list (StdMove l
                                   (Loc (Loc-row lc) (Loc-col l))
                                   (Piece 'Rook (ChessGame-turn cg))
                                   (Some (Piece tp col)))))))])
   (match (third (stoppers 'Rook l (ChessGame-board cg)))
     ['None (build-list (Loc-col l)
                        (lambda ([k : Natural])
                          (StdMove l
                                   (Loc (Loc-row l) (- (Loc-col l) (add1 k)))
                                   (Piece 'Rook (ChessGame-turn cg))
                                   'None)))]
     [(Some (Pr lc (Piece tp col)))
      (local {(define bl1
                (build-list
                 (- (Loc-col l) (add1 (Loc-col lc)))
                 (lambda ([k : Natural])
                   (StdMove l
                            (Loc (Loc-row l) (- (Loc-col l) (add1 k)))
                            (Piece 'Rook (ChessGame-turn cg))
                            'None))))}
        (if (symbol=? (ChessGame-turn cg) col) bl1
            (append bl1
                    (list (StdMove l
                                   (Loc (Loc-row l) (Loc-col lc))
                                   (Piece 'Rook (ChessGame-turn cg))
                                   (Some (Piece tp col)))))))])
   (match (last (stoppers 'Rook l (ChessGame-board cg)))
     ['None (build-list (Loc-row l)
                        (lambda ([k : Natural])
                          (StdMove l
                                   (Loc (- (Loc-row l) (add1 k)) (Loc-col l))
                                   (Piece 'Rook (ChessGame-turn cg))
                                   'None)))]
     [(Some (Pr lc (Piece tp col)))
      (local {(define bl1
                (build-list
                 (- (Loc-row l) (add1 (Loc-row lc)))
                 (lambda ([k : Natural])
                   (StdMove l
                            (Loc (- (Loc-row l) (add1 k)) (Loc-col l))
                            (Piece 'Rook (ChessGame-turn cg))
                            'None))))}
        (if (symbol=? (ChessGame-turn cg) col) bl1
            (append bl1
                    (list (StdMove l
                                   (Loc (Loc-row lc) (Loc-col l))
                                   (Piece 'Rook (ChessGame-turn cg))
                                   (Some (Piece tp col)))))))])))

(check-expect (possible-moves-rook (ChessGame starting-board
                                              'White
                                              '()
                                              (Castles #t #t #t #t))
                                   (Loc 2 3))
              
                (list
                 (StdMove (Loc 2 3) (Loc 2 4) (Piece 'Rook 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 5) (Piece 'Rook 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 6) (Piece 'Rook 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 7) (Piece 'Rook 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 3 3) (Piece 'Rook 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 4 3) (Piece 'Rook 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 5 3) (Piece 'Rook 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 2) (Piece 'Rook 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 1) (Piece 'Rook 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 0) (Piece 'Rook 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 1 3) (Piece 'Rook 'White)
                          (Some (Piece 'Pawn 'Black)))))


(: possible-moves-bishop (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical bishop in the given location
;; can make for the player whose turn it is
;; without considering whether the move puts the player in check.
   
(define (possible-moves-bishop cg l)
  (local {(define col (Loc-col l))
          (define row (Loc-row l))
          (define brd (ChessGame-board cg))
          (define trn (ChessGame-turn cg))
          (define pair/ (/pair l brd))
          (define pair\ (\pair l brd))}      
    (remove* (list (StdMove l l (Piece 'Bishop 'White) 'None))
             (append
              (match* ((first (stoppers 'Bishop l brd))
                       (third (stoppers 'Bishop l brd)))
                [('None 'None) (map (lambda ([pr : (Pr Loc Square)])
                                      (StdMove l (Pr-first pr)
                                               (Piece 'Bishop trn) 'None))
                                    pair/)]              
                [((Some (Pr lc (Piece tp clr))) 'None)
                 (map (lambda ([pr : (Pr Loc Square)])
                        (StdMove l (Pr-first pr) (Piece 'Bishop trn)
                                 (if (and (= (Loc-row (Pr-first pr))
                                             (Loc-row lc))
                                          (= (Loc-col (Pr-first pr))
                                             (Loc-col lc)))
                                     (Some (Piece tp clr)) 'None)))
                      (filter (lambda ([pr : (Pr Loc Square)])
                                       (if (symbol=? clr trn)
                                           (< (Loc-col (Pr-first pr))
                                                                 (Loc-col lc))
                                           (<= (Loc-col (Pr-first pr))
                                               (Loc-col lc)))) pair/))]
                       [('None (Some (Pr lc (Piece tp clr))))
                        (map (lambda ([pr : (Pr Loc Square)])
                               (StdMove l (Pr-first pr) (Piece 'Bishop trn)
                                        (if (and (= (Loc-row (Pr-first pr))
                                                    (Loc-row lc))
                                                 (= (Loc-col (Pr-first pr))
                                                    (Loc-col lc)))
                                            (Some (Piece tp clr)) 'None)))
                             (filter (lambda ([pr : (Pr Loc Square)])
                                       (if (symbol=? clr trn) (> (Loc-col
                                                                  (Pr-first pr))
                                                                 (Loc-col lc))
                                           (>= (Loc-col (Pr-first pr))
                                               (Loc-col lc)))) pair/))]
                       [((Some (Pr lc1 (Piece tp1 clr1)))
                         (Some (Pr lc2
                                   (Piece tp2 clr2))))
                        (map (lambda ([pr : (Pr Loc Square)])
                               (StdMove l (Pr-first pr) (Piece 'Bishop trn)
                                        (cond
                                          [(and (= (Loc-row (Pr-first pr))
                                                   (Loc-row lc1))
                                                (= (Loc-col (Pr-first pr))
                                                   (Loc-col lc1)))
                                           (Some (Piece tp1 clr1))]
                                          [(and (= (Loc-row (Pr-first pr))
                                                   (Loc-row lc2))
                                                (= (Loc-col (Pr-first pr))
                                                   (Loc-col lc2)))
                                           (Some (Piece tp2 clr2))]
                                          [else 'None])))
                             (filter (lambda ([pr : (Pr Loc Square)])
                                       (cond
                                         [(and (symbol=? clr1 trn)
                                               (symbol=? clr2 trn))
                                          (and (> (Loc-col (Pr-first pr))
                                                  (Loc-col lc2))
                                               (< (Loc-col (Pr-first pr))
                                                  (Loc-col lc1)))]
                                         [(and (symbol=? clr1 trn)
                                               (not
                                                (symbol=? clr2 trn)))
                                          (and (>= (Loc-col (Pr-first pr))
                                                   (Loc-col lc2))
                                               (< (Loc-col (Pr-first pr))
                                                  (Loc-col lc1)))]
                                         [(and (not (symbol=? clr1 trn))
                                               (symbol=? clr2 trn))
                                          (and (> (Loc-col (Pr-first pr))
                                                  (Loc-col lc2))
                                               (<= (Loc-col (Pr-first pr))
                                                   (Loc-col lc1)))]
                                         [else (and (>= (Loc-col (Pr-first pr))
                                                        (Loc-col lc2))
                                                    (<= (Loc-col (Pr-first pr))
                                                        (Loc-col lc1)))]))
                                     pair/))])
                     (match* ((second (stoppers 'Bishop l brd))
                              (last (stoppers 'Bishop l brd)))
                       [('None 'None) (map (lambda ([pr : (Pr Loc Square)])
                                             (StdMove l
                                                      (Pr-first pr)
                                                      (Piece 'Bishop trn)
                                                      'None)) pair\ )]
                       [((Some (Pr lc (Piece tp clr))) 'None)
                        (map (lambda ([pr : (Pr Loc Square)])
                               (StdMove l (Pr-first pr) (Piece 'Bishop trn)
                                        (if (and (=
                                                  (Loc-row (Pr-first pr))
                                                  (Loc-row lc))
                                                 (= (Loc-col (Pr-first pr))
                                                    (Loc-col lc)))
                                            (Some (Piece tp clr)) 'None)))
                             (filter (lambda ([pr : (Pr Loc Square)])
                                       (if (symbol=? clr trn) (< (Loc-col
                                                                  (Pr-first pr))
                                                                 (Loc-col lc))
                                           (<= (Loc-col (Pr-first pr))
                                               (Loc-col lc)))) pair\ ))]
                       [('None (Some (Pr lc (Piece tp clr))))
                        (map (lambda ([pr : (Pr Loc Square)])
                               (StdMove l (Pr-first pr) (Piece 'Bishop trn)
                                        (if (and (= (Loc-row
                                                     (Pr-first pr))
                                                    (Loc-row lc))
                                                 (= (Loc-col (Pr-first pr))
                                                    (Loc-col lc)))
                                            (Some (Piece tp clr)) 'None)))
                             (filter (lambda ([pr : (Pr Loc Square)])
                                       (if (symbol=? clr trn) (> (Loc-col
                                                                  (Pr-first pr))
                                                                 (Loc-col lc))
                                           (>= (Loc-col (Pr-first pr))
                                               (Loc-col lc)))) pair\ ))]
                       [((Some (Pr lc1 (Piece tp1 clr1)))
                         (Some (Pr lc2
                                   (Piece tp2 clr2))))
                        (map (lambda ([pr : (Pr Loc Square)])
                               (StdMove l (Pr-first pr) (Piece 'Bishop trn)
                                        (cond
                                          [(and (= (Loc-row (Pr-first pr))
                                                   (Loc-row lc1))
                                                (= (Loc-col (Pr-first pr))
                                                   (Loc-col lc1)))
                                           (Some (Piece tp1 clr1))]
                                          [(and (= (Loc-row (Pr-first pr))
                                                   (Loc-row lc2))
                                                (= (Loc-col (Pr-first pr))
                                                   (Loc-col lc2)))
                                           (Some (Piece tp2 clr2))]
                                          [else 'None])))
                             (filter (lambda ([pr : (Pr Loc Square)])
                                       (cond
                                         [(and (symbol=? clr1 trn)
                                               (symbol=? clr2 trn))
                                          (and (> (Loc-col (Pr-first pr))
                                                  (Loc-col lc2))
                                               (< (Loc-col (Pr-first pr))
                                                  (Loc-col lc1)))]
                                         [(and (symbol=? clr1 trn)
                                               (not
                                                (symbol=? clr2 trn)))
                                          (and (>= (Loc-col (Pr-first pr))
                                                   (Loc-col lc2))
                                               (< (Loc-col (Pr-first pr))
                                                  (Loc-col lc1)))]
                                         [(and (not (symbol=? clr1 trn))
                                               (symbol=? clr2 trn))
                                          (and (> (Loc-col (Pr-first pr))
                                                  (Loc-col lc2))
                                               (<= (Loc-col (Pr-first pr))
                                                   (Loc-col lc1)))]
                                         [else (and (>= (Loc-col (Pr-first pr))
                                                        (Loc-col lc2))
                                                    (<= (Loc-col (Pr-first pr))
                                                        (Loc-col lc1)))]))
                                     pair\ ))])))))

(check-expect (sort-moves (possible-moves-bishop (ChessGame starting-board
                                              'White
                                              '()
                                              (Castles #t #t #t #t))
                                   (Loc 2 3)))
                (sort-moves
                 (list
                 (StdMove (Loc 2 3) (Loc 1 4) (Piece 'Bishop 'White)
                          (Some (Piece 'Pawn 'Black)))
                 (StdMove (Loc 2 3) (Loc 3 4) (Piece 'Bishop 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 4 5) (Piece 'Bishop 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 5 6) (Piece 'Bishop 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 3 2) (Piece 'Bishop 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 4 1) (Piece 'Bishop 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 5 0) (Piece 'Bishop 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 1 2) (Piece 'Bishop 'White)
                          (Some (Piece 'Pawn 'Black))))))
                          
                
(: possible-moves-queen (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical queen in the given location
;; can make for the player whose turn it is
;; without considering whether the move puts the player in check.

(define (possible-moves-queen cg l)
  (map (lambda ([std : Move])
         (match std
           [(StdMove s d _ c) (StdMove s d (Piece 'Queen (ChessGame-turn cg)) c)]))
       (append (possible-moves-rook cg l) (possible-moves-bishop cg l))))


(check-expect (sort-moves (possible-moves-queen (ChessGame starting-board
                                              'White
                                              '()
                                              (Castles #t #t #t #t))
                                   (Loc 2 3)))
                (sort-moves
                 (list
                 (StdMove (Loc 2 3) (Loc 1 4) (Piece 'Queen 'White)
                          (Some (Piece 'Pawn 'Black)))
                 (StdMove (Loc 2 3) (Loc 3 4) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 4 5) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 5 6) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 3 2) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 4 1) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 5 0) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 1 2) (Piece 'Queen 'White)
                          (Some (Piece 'Pawn 'Black)))
                 (StdMove (Loc 2 3) (Loc 2 4) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 5) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 6) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 7) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 3 3) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 4 3) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 5 3) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 2) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 1) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 2 0) (Piece 'Queen 'White) 'None)
                 (StdMove (Loc 2 3) (Loc 1 3) (Piece 'Queen 'White)
                          (Some (Piece 'Pawn 'Black))))))


(: kinglocs (Loc -> (Listof Loc)))
;; Produces a list of all locations in the square perimeter around the given location.
;; Does not take into consideration the boundaries of the board.

(define (kinglocs l)
  (local {(define r (Loc-row l))
          (define c (Loc-col l))}
    (list (Loc (add1 r) c)
          (Loc (add1 r) (add1 c))
          (Loc r (add1 c))
          (Loc (add1 r) (sub1 c))
          (Loc (sub1 r) (sub1 c))
          (Loc (sub1 r) (add1 c))
          (Loc (sub1 r) c)
          (Loc r (sub1 c)))))
(check-expect
 (knightlocs (Loc 3 4))
 (list (Loc 5 5) (Loc 5 3) (Loc 4 6) (Loc 4 2)
       (Loc 2 6) (Loc 2 2) (Loc 1 5) (Loc 1 3))) 


(: knightlocs (Loc -> (Listof Loc)))
;; Produces a list of all locations with one entry differing by 2, the other by 1,
;; from entries of given location.

(define (knightlocs l)
  (local {(define r (Loc-row l))
          (define c (Loc-col l))}
    (list (Loc (+ 2 r) (add1 c))
          (Loc (+ 2 r) (sub1 c))
          (Loc (add1 r) (+ 2 c))
          (Loc (add1 r) (- c 2))
          (Loc (sub1 r) (+ c 2))
          (Loc (sub1 r) (- c 2))
          (Loc (- r 2) (add1 c))
          (Loc (- r 2) (sub1 c)))))

  
(: possible-moves-king-knight ((U 'Knight 'King) ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical piece (king or knight)
;; in the given location can make for the player whose turn it is
;; without considering whether the move puts the player in check.                                

(define (possible-moves-king-knight k cg l)
  (map (lambda ([loc : Loc])
         (match (board-ref (ChessGame-board cg) loc)
           ['None (StdMove l loc (Piece k (ChessGame-turn cg)) 'None)]
           [(Some (Piece t col))
            (StdMove l loc (Piece k (ChessGame-turn cg)) (Some (Piece t col)))]))
       (filter (lambda ([lc : Loc])
                 (and (>= (Loc-col lc) 0)
                      (< (Loc-col lc) 8)
                      (>= (Loc-row lc) 0)
                      (< (Loc-row lc) 8)
                      (match (board-ref (ChessGame-board cg) lc)
                        ['None #t]
                        [(Some (Piece _ p)) (not
                                             (symbol=? p
                                                       (ChessGame-turn cg)))])))
               (if (symbol=? k 'King) (kinglocs l) (knightlocs l)))))
           
    
(check-expect (sort-moves
               (possible-moves-king-knight 'King
                                           (ChessGame starting-board
                                              'Black
                                              '()
                                              (Castles #t #t #t #t))
                                           (Loc 6 6)))
              (sort-moves
               (list
                (StdMove (Loc 6 6) (Loc 6 7) (Piece 'King 'Black)
                         (Some (Piece 'Pawn 'White)))
                (StdMove (Loc 6 6) (Loc 7 7) (Piece 'King 'Black)
                         (Some (Piece 'Rook 'White)))
                (StdMove (Loc 6 6) (Loc 7 6) (Piece 'King 'Black)
                         (Some (Piece 'Knight 'White)))
                (StdMove (Loc 6 6) (Loc 7 5) (Piece 'King 'Black)
                         (Some (Piece 'Bishop 'White)))
                (StdMove (Loc 6 6) (Loc 6 5) (Piece 'King 'Black)
                         (Some (Piece 'Pawn 'White)))
                (StdMove (Loc 6 6) (Loc 5 5) (Piece 'King 'Black) 'None)
                (StdMove (Loc 6 6) (Loc 5 6) (Piece 'King 'Black) 'None)
                (StdMove (Loc 6 6) (Loc 5 7) (Piece 'King 'Black) 'None))))



(: pmpb2/3/5 (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical black pawn in the given location
;; with row 2, 3, or 5 can make for the player whose turn it is
;; without considering whether the move puts the player in check.

(define (pmpb2/3/5 cg l)
  (local {(define forward (Loc (add1 (Loc-row l)) (Loc-col l)))
          (define diagleft (Loc (add1 (Loc-row l)) (sub1 (Loc-col l))))
          (define diagright (Loc (add1 (Loc-row l)) (add1 (Loc-col l))))
          (define colleft (Loc-col diagleft))
          (define colright (Loc-col diagright))
          (define b (ChessGame-board cg))}
    (append (if (symbol? (board-ref b forward))
                (list (StdMove l forward (Piece 'Pawn 'Black) 'None))
                '())
            (if (>= colleft 0) 
                (match (board-ref b diagleft)
                  ['None '()]
                  [(Some (Piece tp col))
                   (if (symbol=? col 'White)
                       (list (StdMove l diagleft
                                (Piece 'Pawn 'Black)
                                (Some (Piece tp col)))) '())]) '())                                                  
            (if (<= colright 7) 
                (match (board-ref b diagright)
                  ['None '()]
                  [(Some (Piece tp col))
                   (if (symbol=? col 'White)
                       (list (StdMove l diagright
                                (Piece 'Pawn 'Black)
                                (Some (Piece tp col)))) '())]) '()))))

(: pmpb1 (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical black pawn in the given location
;; with row 1 can make for the player whose turn it is
;; without considering whether the move puts the player in check.

(define (pmpb1 cg l)
  (append (pmpb2/3/5 cg l)
          (if (symbol? (board-ref (ChessGame-board cg)
                                  (Loc (+ 2 (Loc-row l)) (Loc-col l))))
              (list (StdMove l (Loc (+ 2 (Loc-row l)) (Loc-col l))
                                    (Piece 'Pawn 'Black) 'None)) '())))

(: pmpb4 (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical black pawn in the given location
;; with row 4 can make for the player whose turn it is
;; without considering whether the move puts the player in check.

(define (pmpb4 cg l)
  (append (pmpb2/3/5 cg l)
          (match (ChessGame-history cg)
            ['() '()]
            [(cons a rs)
             (match a
               [(StdMove st dst moved capt)
                (cond
                  [(and (symbol=? (Piece-type moved) 'Pawn)
                        (= (- (Loc-row st) (Loc-row dst)) 2)
                        (or (= (Loc-col st) (add1 (Loc-col l)))
                            (= (Loc-col st) (sub1 (Loc-col l)))))
                   (list (StdMove l (Loc (add1 (Loc-row dst)) (Loc-col dst))
                            (Piece 'Pawn 'Black)
                            (Some (Piece 'Pawn 'White))))]
                  [else '()])]
               [(CastleMove _ _ _ _ _) '()]
               [(PromoMove _ _ _ _ _) '()])])))

(: pmpb6 (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical black pawn in the given location
;; with row 6 can make for the player whose turn it is
;; without considering whether the move puts the player in check.

(define (pmpb6 cg l)
  (local {(define forward (Loc (add1 (Loc-row l)) (Loc-col l)))
          (define diagleft (Loc (add1 (Loc-row l)) (sub1 (Loc-col l))))
          (define diagright (Loc (add1 (Loc-row l)) (add1 (Loc-col l))))
          (define colleft (Loc-col diagleft))
          (define colright (Loc-col diagright))
          (define b (ChessGame-board cg))}
    (append (if (symbol? (board-ref b forward))
                (list (PromoMove l forward (Piece 'Pawn 'Black) 'None 'Queen)
                      (PromoMove l forward (Piece 'Pawn 'Black) 'None 'Rook)
                      (PromoMove l forward (Piece 'Pawn 'Black) 'None 'Bishop)
                      (PromoMove l forward (Piece 'Pawn 'Black) 'None 'Knight))
                '())
            (if (>= colleft 0) 
                (match (board-ref b diagleft)
                  ['None '()]
                  [(Some (Piece tp col))
                   (if (symbol=? col 'White)
                       (list
                        (PromoMove l diagleft
                                (Piece 'Pawn 'Black)
                                (Some (Piece tp col))
                                'Queen)
                        (PromoMove l diagleft
                                (Piece 'Pawn 'Black)
                                (Some (Piece tp col))
                                'Rook)
                        (PromoMove l diagleft
                                (Piece 'Pawn 'Black)
                                (Some (Piece tp col))
                                'Bishop)
                        (PromoMove l diagleft
                                (Piece 'Pawn 'Black)
                                (Some (Piece tp col))
                                'Knight)) '())]) '())                                                  
            (if (<= colright 7) 
                (match (board-ref b diagright)
                  ['None '()]
                  [(Some (Piece tp col))
                   (if (symbol=? col 'White)
                       (list (PromoMove l diagright
                                (Piece 'Pawn 'Black)
                                (Some (Piece tp col))
                                'Queen)
                             (PromoMove l diagright
                                (Piece 'Pawn 'Black)
                                (Some (Piece tp col))
                                'Rook)
                             (PromoMove l diagright
                                (Piece 'Pawn 'Black)
                                (Some (Piece tp col))
                                'Bishop)
                             (PromoMove l diagright
                                (Piece 'Pawn 'Black)
                                (Some (Piece tp col))
                                'Knight)) '())]) '()))))


(: pmpw5/4/2 (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical white pawn in the given location
;; with row 2, 4, or 5 can make for the player whose turn it is
;; without considering whether the move puts the player in check.

(define (pmpw5/4/2 cg l)
  (local {(define forward (Loc (sub1 (Loc-row l)) (Loc-col l)))
          (define diagleft (Loc (sub1 (Loc-row l)) (sub1 (Loc-col l))))
          (define diagright (Loc (sub1 (Loc-row l)) (add1 (Loc-col l))))
          (define colleft (Loc-col diagleft))
          (define colright (Loc-col diagright))
          (define b (ChessGame-board cg))}
    (append (if (symbol? (board-ref b forward))
                (list (StdMove l forward (Piece 'Pawn 'White) 'None))
                '())
            (if (>= colleft 0) 
                (match (board-ref b diagleft)
                  ['None '()]
                  [(Some (Piece tp col))
                   (if (symbol=? col 'Black)
                       (list (StdMove l diagleft
                                (Piece 'Pawn 'White)
                                (Some (Piece tp col)))) '())]) '())                                                  
            (if (<= colright 7) 
                (match (board-ref b diagright)
                  ['None '()]
                  [(Some (Piece tp col))
                   (if (symbol=? col 'Black)
                       (list (StdMove l diagright
                                (Piece 'Pawn 'White)
                                (Some (Piece tp col)))) '())]) '()))))


(: pmpw6 (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical white pawn in the given location
;; with row 6 can make for the player whose turn it is
;; without considering whether the move puts the player in check.

(define (pmpw6 cg l)
  (append (pmpw5/4/2 cg l)
          (if (symbol? (board-ref (ChessGame-board cg)
                                  (Loc (- (Loc-row l) 2) (Loc-col l))))
              (list (StdMove l (Loc (- (Loc-row l) 2) (Loc-col l))
                                    (Piece 'Pawn 'White) 'None)) '())))

(: pmpw3 (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical white pawn in the given location
;; with row 3 can make for the player whose turn it is
;; without considering whether the move puts the player in check.

(define (pmpw3 cg l)
  (append (pmpw5/4/2 cg l)
          (match (ChessGame-history cg)
            ['() '()]
            [(cons a rs)
             (match a
               [(StdMove st dst moved capt)
                (cond
                  [(and (symbol=? (Piece-type moved) 'Pawn)
                        (= (- (Loc-row st) (Loc-row dst)) -2)
                        (or (= (Loc-col st) (add1 (Loc-col l)))
                            (= (Loc-col st) (sub1 (Loc-col l)))))
                   (list (StdMove l (Loc (sub1 (Loc-row dst)) (Loc-col dst))
                            (Piece 'Pawn 'White) (Some (Piece 'Pawn 'Black))))]
                  [else '()])]
               [(CastleMove _ _ _ _ _) '()]
               [(PromoMove _ _ _ _ _) '()])])))



(: pmpw1 (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical white pawn in the given location
;; with row 1 can make for the player whose turn it is
;; without considering whether the move puts the player in check.

(define (pmpw1 cg l)
  (local {(define forward (Loc (sub1 (Loc-row l)) (Loc-col l)))
          (define diagleft (Loc (sub1 (Loc-row l)) (sub1 (Loc-col l))))
          (define diagright (Loc (sub1 (Loc-row l)) (add1 (Loc-col l))))
          (define colleft (Loc-col diagleft))
          (define colright (Loc-col diagright))
          (define b (ChessGame-board cg))}
    (append (if (symbol? (board-ref b forward))
                (list (PromoMove l forward (Piece 'Pawn 'White) 'None 'Queen)
                      (PromoMove l forward (Piece 'Pawn 'White) 'None 'Rook)
                      (PromoMove l forward (Piece 'Pawn 'White) 'None 'Bishop)
                      (PromoMove l forward (Piece 'Pawn 'White) 'None 'Knight))
                '())
            (if (>= colleft 0) 
                (match (board-ref b diagleft)
                  ['None '()]
                  [(Some (Piece tp col))
                   (if (symbol=? col 'Black)
                       (list
                        (PromoMove l diagleft
                                (Piece 'Pawn 'White)
                                (Some (Piece tp col))
                                'Queen)
                        (PromoMove l diagleft
                                (Piece 'Pawn 'White)
                                (Some (Piece tp col))
                                'Rook)
                        (PromoMove l diagleft
                                (Piece 'Pawn 'White)
                                (Some (Piece tp col))
                                'Bishop)
                        (PromoMove l diagleft
                                (Piece 'Pawn 'White)
                                (Some (Piece tp col))
                                'Knight)) '())]) '())                                                  
            (if (<= colright 7) 
                (match (board-ref b diagright)
                  ['None '()]
                  [(Some (Piece tp col))
                   (if (symbol=? col 'Black)
                       (list (PromoMove l diagright
                                (Piece 'Pawn 'White)
                                (Some (Piece tp col))
                                'Queen)
                             (PromoMove l diagright
                                (Piece 'Pawn 'White)
                                (Some (Piece tp col))
                                'Rook)
                             (PromoMove l diagright
                                (Piece 'Pawn 'White)
                                (Some (Piece tp col))
                                'Bishop)
                             (PromoMove l diagright
                                (Piece 'Pawn 'White)
                                (Some (Piece tp col))
                                'Knight)) '())]) '()))))

(: possible-moves-pawn (ChessGame Loc -> (Listof Move)))
;; Lists all the moves that a hypothetical pawn in the given location
;; can make for the player whose turn it is
;; without considering whether the move puts the player in check.

(define (possible-moves-pawn cg l)
  (if (symbol=? (ChessGame-turn cg) 'White)
      (cond
       [(= (Loc-row l) 1) (pmpw1 cg l)]
       [(= (Loc-row l) 6) (pmpw6 cg l)]
       [(= (Loc-row l) 3) (pmpw3 cg l)]
       [else (pmpw5/4/2 cg l)])
      (cond
        [(= (Loc-row l) 1) (pmpb1 cg l)]
        [(= (Loc-row l) 6) (pmpb6 cg l)]
        [(= (Loc-row l) 4) (pmpb4 cg l)]
        [else (pmpb2/3/5 cg l)])))


(: possible-moves-piece (ChessGame Loc -> (Listof Move)))
;; Lists all (non-castling) moves that can be made from the given location
;; in the given chess game without considering whether move puts
;; player in check.

(define (possible-moves-piece cg l)
  (local {(define trn (ChessGame-turn cg))
          (define brd (ChessGame-board cg))}
    (match (board-ref brd l)
      ['None '()]
      [(Some (Piece tp col))
       (if (symbol=? col trn)
           (cond [(symbol=? tp 'Rook) (possible-moves-rook cg l)]
                 [(symbol=? tp 'Bishop) (possible-moves-bishop cg l)]
                 [(symbol=? tp 'Queen) (possible-moves-queen cg l)]
                 [(symbol=? tp 'Knight)
                  (possible-moves-king-knight 'Knight cg l)]
                 [(symbol=? tp 'King) (possible-moves-king-knight 'King cg l)]
                 [(symbol=? tp 'Pawn) (possible-moves-pawn cg l)]
                 [else '()])
           '())])))

(: possible-moves-player (ChessGame -> (Listof Move)))
;; List all (non-castling) moves that can be made in given game without considering
;; whether the move puts the player in check.

(define (possible-moves-player cg)
  (match cg
    [(ChessGame b t h c)
     (match b
       ['() '()]
       [(cons row rows)
        (foldl (inst append Move)
               '()
               (build-list
                8
                (lambda
                    ([n : Natural])
                  (foldl
                   (inst append Move)
                   '()
                   (build-list 8
                               (lambda ([k : Natural])
                                 (possible-moves-piece cg (Loc n k))))))))])]))
  

(: kingcaptured ((Listof Move) -> Boolean))
;; Checks whether a king is captured in a list of moves.

(define (kingcaptured ms)
  (match ms
    ['() #f]
    [(cons m rs) (match m
                   [(StdMove _ _ _ cap)
                    (match cap
                      ['None (kingcaptured rs)]
                      [(Some (Piece t c)) (or (symbol=? 'King t)
                                              (kingcaptured rs))])]
                   [(PromoMove _ _ _ cap _)
                    (match cap
                      ['None (kingcaptured rs)]
                      [(Some (Piece t c)) (or (symbol=? 'King t)
                                              (kingcaptured rs))])])]))
                                                   
(: in-check? (ChessGame -> Boolean))
;; The function returns true if and only if the player whose turn
;; it is is in check.

(define (in-check? cg)
  (match cg
    [(ChessGame b t h c)
     (kingcaptured 
      (possible-moves-player
       (ChessGame b
                  (if (symbol=? t 'Black) 'White 'Black)
                  h
                  c)))]))
(provide in-check?)

(: movecontains? ((Listof Move) Move -> Boolean))
;; Checks whether a list of moves contains the given move.

(define (movecontains? ms m)
  (match ms
    ['() #f]
    [(cons a rs)
     (match* (a m) 
       [((StdMove s1 d1 p1 c1) (StdMove s2 d2 p2 c2))
        (or (and (= (Loc-row s1) (Loc-row s2))
                 (= (Loc-col s1) (Loc-col s2))
                 (= (Loc-row d1) (Loc-row d2))
                 (= (Loc-col d1) (Loc-col d2))
                 (symbol=? (Piece-type p1) (Piece-type p2))
                 (symbol=? (Piece-color p1) (Piece-color p2))
                 (match* (c1 c2)
                   [('None 'None) #t]
                   [('None (Some _)) #f]
                   [((Some _) 'None) #f]
                   [((Some (Piece t1 col1)) (Some (Piece t2 col2)))
                    (and (symbol=? t1 t2) (symbol=? col1 col2))]))
            (movecontains? rs m))]
       [((StdMove _ _ _ _) (PromoMove _ _ _ _ _))
        (movecontains? rs m)]
       [((StdMove _ _ _ _) (CastleMove _ _ _ _ _))
        (movecontains? rs m)]
       [((PromoMove _ _ _ _ _) (StdMove _ _ _ _))
        (movecontains? rs m)]
       [((PromoMove s1 d1 p1 c1 _) (PromoMove s2 d2 p2 c2 _))
        ;;we assume that if the list has the same promotion move but
        ;;but with a different promote-to piece, it has the same
        ;;promotion move
        (or (and (= (Loc-row s1) (Loc-row s2))
                 (= (Loc-col s1) (Loc-col s2))
                 (= (Loc-row d1) (Loc-row d2))
                 (= (Loc-col d1) (Loc-col d2))
                 (symbol=? (Piece-type p1) (Piece-type p2))
                 (symbol=? (Piece-color p1) (Piece-color p2))
                 (match* (c1 c2)
                   [('None 'None) #t]
                   [('None (Some _)) #f]
                   [((Some _) 'None) #f]
                   [((Some (Piece t1 col1)) (Some (Piece t2 col2)))
                    (and (symbol=? t1 t2) (symbol=? col1 col2))]))
            (movecontains? rs m))]
       [((PromoMove _ _ _ _ _) (CastleMove _ _ _ _ _))
        (movecontains? rs m)]
       [((CastleMove _ _ _ _ _) (StdMove _ _ _ _))
        (movecontains? rs m)]
       [((CastleMove _ _ _ _ _) (PromoMove _ _ _ _ _))
        (movecontains? rs m)]
       [((CastleMove ks1 _ rs1 _ _) (CastleMove ks2 _ rs2 _ _))
        (or (and (= (Loc-row ks1) (Loc-row ks2))
                 (= (Loc-col rs1) (Loc-col rs2)))
            (movecontains? rs m))])]))


                                    
(: possible-castles-piece (ChessGame Loc -> (Listof Move)))
;; Lists all the castle-moves that can occur,
;; in which a piece at the given location is moved.
;; Does not take into account whether move results in check.

(define (possible-castles-piece cg l)
  (local {(define b (ChessGame-board cg))
          (define trn (ChessGame-turn cg))
          (define his (ChessGame-history cg))
          (define cs (ChessGame-cas cg))
          (define col (Loc-col l))
          (define row (Loc-row l))
          (define 0-0 (Castles-black-toward-0-0 (ChessGame-cas cg)))
          (define 0-7 (Castles-black-toward-0-7 (ChessGame-cas cg)))
          (define 7-0 (Castles-white-toward-7-0 (ChessGame-cas cg)))
          (define 7-7 (Castles-white-toward-7-7 (ChessGame-cas cg)))
          (define move0-0 (list (CastleMove (Loc 0 4)
                                            (Loc 0 2)
                                            (Loc 0 0)
                                            (Loc 0 3)
                                            'Black)))
          (define move0-7 (list (CastleMove (Loc 0 4)
                                            (Loc 0 6)
                                            (Loc 0 7)
                                            (Loc 0 5)
                                            'Black)))
          (define bool0-0 (and 0-0
                               (symbol? (board-ref b (Loc 0 1)))
                               (symbol? (board-ref b (Loc 0 2)))
                               (symbol? (board-ref b (Loc 0 3)))
                               (not (in-check? cg))
                               (not (in-check?
                                     (ChessGame
                                      (board-update b (Loc 0 3)
                                                    (Some (Piece 'King 'Black)))
                                      trn his cs)))))
          (define bool0-7 (and 0-7
                               (symbol? (board-ref b (Loc 0 5)))
                               (symbol? (board-ref b (Loc 0 6)))
                               (not (in-check? cg))
                               (not (in-check?
                                     (ChessGame
                                      (board-update b (Loc 0 5)
                                                    (Some (Piece 'King 'Black)))
                                      trn his cs)))))
          (define move7-0 (list (CastleMove (Loc 7 4)
                                            (Loc 7 2)
                                            (Loc 7 0)
                                            (Loc 7 3)
                                            'White)))
          (define move7-7 (list (CastleMove (Loc 7 4)
                                            (Loc 7 6)
                                            (Loc 7 7)
                                            (Loc 7 5)
                                            'White)))
          (define bool7-0 (and 7-0
                               (symbol? (board-ref b (Loc 7 1)))
                               (symbol? (board-ref b (Loc 7 2)))
                               (symbol? (board-ref b (Loc 7 3)))
                               (not (in-check? cg))
                               (not (in-check?
                                     (ChessGame
                                      (board-update b (Loc 7 3)
                                                    (Some (Piece 'King 'White)))
                                      trn his cs)))))
          (define bool7-7 (and 7-7
                               (symbol? (board-ref b (Loc 7 5)))
                               (symbol? (board-ref b (Loc 7 6)))
                               (not (in-check? cg))
                               (not (in-check?
                                     (ChessGame
                                      (board-update b (Loc 7 5)
                                                    (Some (Piece 'King 'White)))
                                      trn his cs)))))}
    (if (symbol=? trn 'Black)
        (cond
          [(and (= row 0) (= col 4))
           (append
            (if bool0-0 move0-0 '())
            (if bool0-7 move0-7 '()))]             
          [(and (= row 0) (= col 0))
           (if bool0-0 move0-0 '())]
          [(and (= row 0) (= col 7))
           (if bool0-7 move0-7 '())]
          [else '()])
        (cond
          [(and (= row 7) (= col 4))
           (append
            (if bool7-0 move7-0 '())
            (if bool7-7 move7-7 '()))]
          [(and (= row 7) (= col 0))
           (if bool7-0 move7-0 '())]
          [(and (= row 7) (= col 7))
           (if bool7-7 move7-7 '())]
          [else '()]))))

(: possible-castles-player (ChessGame -> (Listof Move)))
;: Lists all castling moves that can be made in given game without considering
;; whether the move puts the player in check.

(define (possible-castles-player cg)
  (if (symbol=? (ChessGame-turn cg) 'White)
       (possible-castles-piece cg (Loc 7 4))
       (possible-castles-piece cg (Loc 0 4))))
       
            

(: apply-move* (ChessGame Move -> ChessGame))
;; Advances the chess game by applying the move.
;; Move may put player under check.

(define (apply-move* cg m)
  (if (movecontains? (append (possible-castles-player cg)
                             (possible-moves-player cg)) m)
      (match* (cg m)
        [((ChessGame b t his cas) (StdMove st dst p cap))
         (ChessGame (board-update
                     (board-update
                      (if
                       (and (symbol? (board-ref b dst))
                            (not (symbol? cap)))
                       (board-update b
                                     (Loc (Loc-row st) (Loc-col dst))
                                     'None)                       
                       b) st 'None) dst (Some p))
                    (if (symbol=? t 'Black) 'White 'Black)
                    (cons m his)
                    (match cas
                      [(Castles b1 b2 b3 b4)
                       (cond
                         [(and (= (Loc-row st) 0) (= (Loc-col st) 0))
                          (Castles #f b2 b3 b4)]
                         [(and (= (Loc-row st) 0) (= (Loc-col st) 7))
                          (Castles b1 #f b3 b4)]
                         [(and (= (Loc-row st) 0) (= (Loc-col st) 4))
                          (Castles #f #f b3 b4)]
                         [(and (= (Loc-row st) 7) (= (Loc-col st) 0))
                          (Castles b1 b2 #f b4)]
                         [(and (= (Loc-row st) 7) (= (Loc-col st) 7))
                          (Castles b1 b2 b3 #f)]
                         [(and (= (Loc-row st) 7) (= (Loc-col st) 4))
                          (Castles b1 b2 #f #f)]
                         [else (Castles b1 b2 b3 b4)])]))]               
        [((ChessGame b t his cas) (CastleMove ks kd rs rd mv))
         (ChessGame (board-update
                     (board-update
                      (board-update
                       (board-update b ks 'None)
                       rs 'None)
                      kd (Some (Piece 'King t)))
                     rd (Some (Piece 'Rook t)))
                    (if (symbol=? t 'Black) 'White 'Black)
                    (cons m his)
                    (match cas
                      [(Castles b1 b2 b3 b4)
                       (cond
                         [(and (= 0 (Loc-row rs)) (= 0 (Loc-col rs)))
                          (Castles (not b1) b2 b3 b4)]
                         [(and (= 0 (Loc-row rs)) (= 7 (Loc-col rs)))
                          (Castles b1 (not b2) b3 b4)]
                         [(and (= 7 (Loc-row rs)) (= 0 (Loc-col rs)))
                          (Castles b1 b2 (not b3) b4)]
                         [else (Castles b1 b2 b3 (not b4))])]))]
        [((ChessGame b t his cas) (PromoMove st dst p c pr))
         (ChessGame (board-update (board-update b st 'None) dst
                                  (Some (Piece pr t)))
                    (if (symbol=? t 'Black) 'White 'Black)
                    (cons m his)
                    cas)])
        (error "impossible move")))
                                                     
(: available-moves-piece : (ChessGame Loc -> (Listof Move)))
;; Returns list of moves that can legally be made with the piece (if there is one)
;; at the given location.

(define (available-moves-piece cg l)
  (filter (lambda ([mv : Move])
            (if (in-check? (apply-move* cg mv)) #f #t))
          (append (possible-castles-piece cg l)
                  (possible-moves-piece cg l))))


(: available-moves-player : (ChessGame -> (Listof Move)))
;; Returns list of all legal moves that can be made in given chess game.

(define (available-moves-player cg)
  (filter (lambda ([mv : Move])
            (if (in-check?
                 (match (apply-move* cg mv)
                   [(ChessGame b t his cas)
                    (ChessGame b (ChessGame-turn cg) his cas)])) #f #t))
          (append (possible-castles-player cg)
                  (possible-moves-player cg))))

(: legal-move? : (ChessGame Move -> Boolean))
;; Returns true if and only if the move is a legal move in the given chess game.

(define (legal-move? cg mv)
  (movecontains? (available-moves-player cg) mv))
(provide legal-move?)

(: apply-move : (ChessGame Move -> ChessGame))
;; Advances the chess game by applying the move if it is legal. 

(define (apply-move cg mv)
  (if (legal-move? cg mv)
      (apply-move* cg mv)
      (error "illegal move")))
(provide apply-move)
  
(: checkmate? : (ChessGame -> Boolean))
;; Returns true if and only if player whose turn it is is checkmated.

(define (checkmate? cg)
  (and (in-check? cg)
       (empty? (available-moves-player cg))))
(provide checkmate?)

(: stalemate? : (ChessGame -> Boolean))
;; Returns true if and only if the player whose turn it is is not in check
;; but has no available moves.

(define (stalemate? cg)
  (and (not (in-check? cg))
       (empty? (available-moves-player cg))))
(provide stalemate?)           

(: strings->board : ((Listof String) -> Board))
;; Creates a board based on the given list of strings.

(define (strings->board sts)
  (local {(: string->board* : ((Listof String) -> Board))
          (define (string->board* sts*)
             (match sts*
                   ['() '()]
                   [(cons a rs)
                    (cons (if (= (string-length a) 8)
                              (map
                               (lambda ([ch : Char])
                                     (cond
                                       [(char=? ch #\P)
                                        (Some (Piece 'Pawn 'Black))]
                                       [(char=? ch #\p)
                                        (Some (Piece 'Pawn 'White))]
                                       [(char=? ch #\R)
                                        (Some (Piece 'Rook 'Black))]
                                       [(char=? ch #\r)
                                        (Some (Piece 'Rook 'White))]
                                       [(char=? ch #\B)
                                        (Some (Piece 'Bishop 'Black))]
                                       [(char=? ch #\b)
                                        (Some (Piece 'Bishop 'White))]
                                       [(char=? ch #\N)
                                        (Some (Piece 'Knight 'Black))]
                                       [(char=? ch #\n)
                                        (Some (Piece 'Knight 'White))]
                                       [(char=? ch #\K)
                                        (Some (Piece 'King 'Black))]
                                       [(char=? ch #\k)
                                        (Some (Piece 'King 'White ))]
                                       [(char=? ch #\Q)
                                        (Some (Piece 'Queen 'Black))]
                                       [(char=? ch #\q)
                                        (Some (Piece 'Queen 'White))]
                                       [(char=? ch #\-) 'None]
                                       [else (error "improper character")]))
                                   (string->list a))
                              (error "row without 8 squares"))
                          (string->board* rs))]))}
    (if (= (length sts) 8) (string->board* sts)
        (error "board without 8 rows"))))
                  
            
(check-expect (strings->board (list "RNBQKBNR"
                                    "PPPPPPPP"
                                    "--------"
                                    "--------"
                                    "--------"
                                    "--------"
                                    "pppppppp"
                                    "rnbqkbnr")) starting-board)

(: row->image : (Listof Square) -> Image)
;; Draws row of chessboard.

(define (row->image ss)
  (match ss
    ['() empty-image]
    [(cons a rs)
     (beside
      (match a
        ['None (overlay (square 40 "outline" "black") 
                       (square 40 "solid" (if (even? (length ss))
                                              "beige" "brown")))]
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
           25 "black")
          (square 40 "outline" "black")
          (square 40 "solid" (if (even? (length ss)) "beige" "brown")))])
      (row->image rs))]))


                                      

(: board->image : Board -> Image)
;; Draws board.

(define (board->image b)
  (match b
    ['() empty-image]
    [(cons row rows) (above (row->image row)
                            (board->image rows))]))        
(provide board->image)


(test)   
             

                                                          
                                 






              



              


          
          
                                
                                
       
                                 
  






