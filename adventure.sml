(* Adventurespil i SML *)
type kort = char list list
type ryk = int
type bane = int
datatype retning = Nord | Syd | Ost | Vest
datatype element = Mur | Luft | X | Y | Ukendt
datatype tilstand = IGang of kort * ryk * bane | Slut

(* Hjælpefunktion retvek : retning -> int * int
 * Returnerer retningsvektoren for retningen.
 * Værdierne her er omvendt i forhold til hvad
 * man normalt kunne forestille sig fordi indicer
 * i lister vokser når man bevæger sig "nedad"
 * i den typiske visuelle repræsentation af dem. *)
val retvek =
    fn Nord => (0, ~1)
     | Syd  => (0, 1)
     | Ost  => (1, 0)
     | Vest => (~1, 0)

(* Hjælpefunktion element : char -> element
 * Returnerer hvad et givet tegn symboliserer.
 *)
val element =
    fn #" " => Luft
     | #"|" => Mur
     | #"+" => Mur
     | #"-" => Mur
     | #"#" => Mur
     | #"X" => X
     | #"Y" => Y
     | _    => Ukendt

;load "TextIO";
;open List;
(* Funktion hentkort : string -> kort
 * Returnerer et kort fundet i en fil.
 * Funktionen håndterer ikke filsystem-undtagelser.
 *)
fun hentkort filnavn =
    let
      val fd = TextIO.openIn filnavn
      val s = TextIO.inputAll fd
      val _ = TextIO.closeIn fd
      fun isBreak c = (c = #"\r" orelse c = #"\n")
    in
      map explode (String.tokens isBreak s)
    end

(* Funktion viskort : kort -> unit
 * Udskriver et kort til skærmen set oppefra. *)
fun viskort kort = app (fn r => print (implode r ^ "\n")) kort

(* Hjælpefunktion index : 'a -> 'a list -> int option
 * Returnerer positionen for første forekomst af x i cs.
 *)
fun index x cs =
    let
      (* Benytter en halerekursiv hjælpefunktion *)
      fun indexr [] _ = NONE
	| indexr (c::cs) n =
	  if x = c
	  then SOME n
	  else indexr cs (n+1)
    in
      indexr cs 0
    end

(* Hjælpefunktion pos : char -> kort -> int * int
 * Returnerer hvilken position på kortet hvor X eller Y befinder sig.
 * Antager at der kun findes ét af x på kortet (fx X eller Y).
 *)
fun pos x kort =
    let
      (* Benytter en halerekursiv hjælpefunktion *)
      fun posr [] _ = raise Fail "Kunne ikke finde X!"
	| posr (r::rs) y =
	  case index x r of
	       NONE => posr rs (y+1)
	     | SOME x => (x, y)
    in
      posr kort 0
    end

(* Hjælpefunktion hentTegn : int * int -> kort -> char
 * Returnerer værdien på en position på kortet. I tilfælde
 * af at værdien ikke er defineret på kortet antages luft
 * (symboliseret ved mellemrum).
 *)
fun hentTegn (x, y) kort =
    nth (nth (kort, y), x) handle Subscript => #" "

(* Hjælpefunktion gemTegn : int * int -> char -> kort -> kort
 * Indsætter tegnet t på en position på kortet og returnerer
 * det nye kort hvor tegnet er indsat. *)
fun gemTegn (x, y) t kort =
    let
      val midten  = nth (kort, y)
      val forinden = take (midten, x)
      val efter = drop (midten, x+1)
    in
      concat [ take (kort, y),
	       [ forinden @ [t] @ efter ],
	       drop (kort, y+1) ]
    end

(* Hjælpefunktion sumpar : int * int -> int * int -> int * int
 * Returnerer summen af talpar, ligesom addition af vektorer. *)
fun sumpar (x1, y1) (x2, y2) = (x1+x2, y1+y2)

(* Funktion flyt : kort -> retning -> (kort * element) option
 * Hvis muligt, returnerer kortet og det som X er rykket ind i,
 * efter at X har bevæget sig. Man kan ikke bevæge sig igennem vægge.
 *)
fun flyt kort retning =
    let
      val posX = pos #"X" kort
      val dest = sumpar posX (retvek retning)
    in
      case element (hentTegn dest kort) of
	   Luft => SOME (gemTegn dest #"X" (gemTegn posX #" " kort), Luft)
	 | Y    => SOME (gemTegn dest #"X" (gemTegn posX #" " kort), Y)
	 | _    => NONE
    end

(* Interaktivt spil
 * Startes ved spil (); *)
fun spil () =
    let
      (* Hjælpefunktion der håndterer ryk, hvis de kan lade sig gøre. *)
      fun ryk Slut _ = input Slut
	| ryk (tilstand as IGang (kort, antal_ryk, bane_nr)) retning =
	  case flyt kort retning of
	       SOME (kort', Luft) =>
	         input (IGang (kort', antal_ryk+1, bane_nr))
	     | SOME (kort', Y)    =>
	         (print ("Du brugte " ^ makestring antal_ryk ^ " træk!\n");
		  let val filnavn = makestring (bane_nr + 1) ^ ".txt"
		  in input (IGang (hentkort filnavn, 0, bane_nr + 1))
		  end)
	     | NONE               => (print "Ulovligt træk! Prøv igen!\n";
				      input tilstand)

      (* Hjælpefunktion der håndterer input og deres gyldighed. *)
      and input Slut = print "Tak for spillet!\n"
	| input (tilstand as IGang (kort, antal_ryk, bane_nr)) =
	  (print ("\nDu står i labyrint " ^ makestring bane_nr ^ ".\n");
	   viskort kort;
	   print "Dit valg (N, S, Ø, V, Q): ";
	   case String.tokens Char.isSpace (TextIO.inputLine TextIO.stdIn) of
	        ["Q"] => input Slut
	      | ["N"] => ryk tilstand Nord
	      | ["S"] => ryk tilstand Syd
	      | ["Ø"] => ryk tilstand Ost
	      | ["V"] => ryk tilstand Vest
	      | _     => (print "Ukendt kommando. Prøv igen!\n";
			  input tilstand))
    in
      (print "Ok.\n";
      input (IGang (hentkort "0.txt", 0, 0)))
      handle (Io { cause = SysErr ("No such file or directory", _), ... }) =>
	     input Slut
    end
