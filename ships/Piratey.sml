structure Piratey =
struct
  (* Representation *)

  type quantity = int
  type price = int

  datatype Ship = Ship of { money   : quantity
                          , cannons : quantity
                          , men     : quantity
                          , cargo   : (quantity * Stuff) list }

       and Stuff = Stuff of { name  : string
                            , worth : price * price }

       and Pirate = Pirate of { name      : string
                              , ship      : Ship
                              , interests : Stuff list }


  (* Initial game: 15 men, a cannon and 5 barrels of coffee. *)
  val coffee = Stuff { name = "coffee", worth = (10,30) }

  val me = Ship { money   = 100
                , cannons = 1
                , men     = 15
                , cargo   = [(5, coffee)] }

  (* And the only other pirate is John, who incidentally wants to buy coffee *)
  val him = Pirate { name = "John the Pirate"
                   , ship = Ship { money   = 2000
                                 , cannons = 1
                                 , men     = 10
                                 , cargo   = [(2, coffee)] }
                   , interests = [coffee] }

end
