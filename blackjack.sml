
datatype card =
         Ace | Two | Three | Four | Five
         | Six | Seven | Eight | Nine | Ten
         | Jack | Queen | King

datatype suit = Hearts | Clubs | Diamonds | Spades

fun cardvalue c =
    case c of
        Ace => 11
      | Two => 2
      | Three => 3
      | Four => 4
      | Five => 5
      | Six => 6
      | Seven => 7
      | Eight => 8
      | Nine => 9
      | _ => 10

fun hasAce [] = false
  | hasAce (Ace::_) = true
  | hasAce (_::hand) = hasAce hand

fun removeAce [] = []
  | removeAce (Ace::hand) = hand
  | removeAce (card::hand) = card::removeAce hand

fun handvalue hand =
    let
      fun handvalue' [] = 0
        | handvalue' (c::cs) = cardvalue c + handvalue' cs
      val hv = handvalue' hand
    in
      if hv > 21 andalso hasAce hand
      then handvalue (removeAce hand) + 1
      else hv
    end

;load "Random";

val rng = Random.newgen ()

val rnum = Random.range (0, 10) rng

fun replicate 0 _ = []
  | replicate n x = x :: replicate (n-1) x

(* TODO: Make Cartesian product of card and suit! *)
(* fun hearts card = (card, Hearts) *)

val allcards =
    List.concat (replicate 4 [Ace, Two, Three, Four, Five, Six,
                              Seven, Eight, Nine, Ten, Jack, Queen, King])

(* randomElement [1,2,3,4] ~> (X, ... (- X)) *)

fun rlist xs = Random.range (0, length xs) rng

fun remove (_, []) = raise Domain
  | remove (0, card::library) = library
  | remove (n, card::library) = card::remove (n-1, library);

fun randomtake library =
    let val i = rlist library
        val card = List.nth (library, i)
        val rest = remove (i, library)
    in
      (card, rest)
    end

fun shuffle [] = []
  | shuffle cards =
    let val (c,r) = randomtake cards
    in
      c :: shuffle r
    end

fun wouldhouse hand =
    handvalue hand < 17

fun house (hand, card::library) =
    if wouldhouse hand
    then house (card::hand, library)
    else hand

fun isBlackjack [King, Ace] = true
  | isBlackjack [Ace, King] = true
  | isBlackjack _ = false

fun playerwins (househand, playerhand) =
    let
      val housevalue = handvalue househand
      val playervalue = handvalue playerhand
    in
      if playervalue > 21 then false else
      if playervalue <= 21 andalso housevalue > 21 then true else
      if playervalue <= housevalue then false else true
    end

fun game () =
    let
      fun showWorth hand = print ("Your hand is worth " ^
                                  Int.toString (handvalue hand) ^ "\n")

      fun showHouse hand = print ("House's hand is worth " ^
                                  Int.toString (handvalue hand) ^ "\n")

      fun showLose hand = print ("House wins!\nYour hand was worth " ^
                                 Int.toString (handvalue hand) ^ "\n")

      fun housePlays (playerhand, library) =
          let val househand = house ([], library)
          in (showWorth playerhand;
              showHouse househand;
              if playerwins (househand, playerhand)
              then print "You win!\n"
              else print "House wins!\n")
          end

      fun loop (hand, topcard::library) =
          (print "Stand or draw? ";
           case TextIO.inputLine(TextIO.stdIn) of
               SOME "Stand\n"   => housePlays (hand, topcard::library)
             | SOME "Hit me!\n" =>
               let
                 val newhand = topcard::hand
                 val nhv = handvalue newhand
               in
                 if nhv > 21 then showLose newhand else
                 if nhv = 21 orelse nhv < 21 andalso length newhand = 5
                   then housePlays (newhand, library)
                   else (showWorth newhand; loop (newhand, library))
               end
             | SOME _ => (print "That doesn't make any sense! Type 'Stand' or 'Hit me!'\n";
                          loop (hand, topcard::library))
          )
    in
      loop ([], shuffle allcards)
    end

(* Udvidelser:
 *  - Gør det muligt at spille mod huset
 *  - Gør det muligt at være flere spillere med samme spil kort
 *  - Gør det muligt at splitte spillet hvis man får to ens
 *  - Gør det muligt at genstarte spillet inde i og efter spillet
 *  - Gør det muligt at vise hvilke hænder man faktisk havde (spiller, huset)
 *  - Gør det muligt at skelne mellem klør dame og spar dame (osv.)
 *)
