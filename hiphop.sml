(* Hip Hop navnegenerator
   12. oktober 2013
   Simon Shine <shine@diku.dk>
   http://heltnormalt.dk/truthfacts/2013/10/04
 *)

(* Indlæs tilfældighedsgenerator *)
val _ = load "Random";
val gen = Random.newgen ()

(* Giver et tilfældigt tal i intervallet [n,m) *)
fun rand (n,m) = Random.range (n,m) gen

(* Tager et tilfældigt element fra en liste *)
fun randNth [] = raise Empty
  | randNth xs = List.nth(xs, rand (0, length xs))

(* Tager en tilfældig funktion fra en liste og evaluerer den *)
(* randNthEval : (unit -> 'a) list -> 'a *)

local
  fun big   () = ("Big",   randNth [ aedelmetal_valuta ])
  and lil   () = ("Lil",   randNth [ aedelmetal_valuta, cannabis_slang, tal_bogstav ])
  and young () = ("Young", randNth [ aedelmetal_valuta, skydevaaben  ])
  and fat   () = ("Fat",   randNth [ cannabis_slang, skydevaaben, tal_bogstav ])
  and mc    () = ("MC",    randNth [ skydevaaben, tal_bogstav ])
  and dj    () = ("DJ",    randNth [ skydevaaben, tal_bogstav ])
  and asap  () = ("ASAP",  randNth [ tal_bogstav, asap' ])

  and aedelmetal_valuta () = (randNth [ "Dollar", "Yen", "Gee", "Money", "Ca$h",
                                        "Gold", "Silver", "Platinum", "Bismuth" ],
                              randNth [ "Boy", "Master" ])
  and cannabis_slang    () = (randNth [ "Ganja", "Smokey", "Dope", "Weed" ],
                              randNth [ "Man", "Thug", "King" ])
  and skydevaaben       () = (randNth [ "Magnum", ".50", "High-Caliber", "Glock",
                                        "Machine Gun", "AK47", "Sniper" ],
                              randNth [ "Master", "Rock", "Rocky" ])
  and tal_bogstav       () = (randNth [ "Zero", "Number One", "II", "III", "Five",
                                        "Seven", "Nine", "A", "11", "'D-12'", "13",
                                        "F.", "U-238" ],
                              randNth [ "Boy", "Thug", "Master", "King" ])
  and asap'             () = ("", randNth [ "Rock", "Rocky" ])

  (* Vigtigt: Udskift alle s/S med z/Z eller $ *)
  fun vigtigt s = String.map (fn #"s"  => randNth [#"z", #"$"]
                               | #"S"  => randNth [#"Z", #"$"]
                               | other => other) s
in
  fun navn () =
      let val (prefixS, middleF) = (randNth [big, lil, young, fat, mc, dj, asap]) ()
          val (middleS, suffixS) = middleF ()
          fun continue false = ""
            | continue true  = " A.K.A. " ^ navn ()
      in String.concat [ vigtigt prefixS, " ",
                         vigtigt middleS, " ",
                         vigtigt suffixS,
                         continue (randNth [true, false]) ]
      end
end

val ti_navne = List.tabulate(10, fn _ => navn ())
