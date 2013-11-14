(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
structure Model =
struct
  (* Representation *)

  type quantity = int
  type price = int
  type coordinate = int * int

  datatype Ship = Ship of { money   : quantity
                          , cannons : quantity
                          , men     : quantity
                          , cargo   : (quantity * Stuff) list }

       and Stuff = Stuff of { name  : string
                            , worth : price * price }

       and Pirate = Pirate of { name      : string
                              , ship      : Ship
                              , interests : Stuff list }

       and GameState = GameState { player  : Ship * coordinate
                                 , pirates : (Pirate * coordinate) list }

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


(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
val _ = load "MosGame" (* Until we use Moscow ML structure-mode *)


structure Graphics =
struct

  structure M = MosGame
  structure E = M.Event
  structure D = M.Draw
  structure I = M.Image
(*structure C = M.Colors*)

  val displayWidth = 960
  val displayHeight = 480
  val scaleFactor = 20 (* when drawing geometric figures, how big should they be *)

  val _ = M.init ()
  val disp =  M.Display.create_display (displayWidth, displayHeight)

  fun draw fs =
      app (fn f => ignore $ f ()) fs
      before M.Display.flip disp

  fun drawShip 


end


structure Main =
struct

  (* Start a game *)
  fun run () =
      let val gameState = Model.initialGame ()
      in loop gameState end

  (* The game loop *)
  and loop gameState =
      (Graphics.draw [ Graphics.drawPlayer gameState
                     , 
                     , drawPlayer (getPlayerPos gameState)];
       loop (process gameState gameMap) gameMap)

  (* Game loop *)
  fun process gameState gameMap =
      case E.poll () of
          NONE => gameState
        | SOME E.QuitEvent => (quit (); gameState)
        | SOME (E.KeyboardEvent data) => handleKey data gameState gameMap
        | SOME _ => gameState

  and handleKey (state, symb, modifiers) gameState gameMap =
      if state <> E.KeyPressed then gameState else
      case symb of
          E.KeyQ     => (quit (); gameState)
        | E.KeyW     => move (0,~1) gameState gameMap
        | E.KeyUp    => move (0,~1) gameState gameMap
        | E.KeyA     => move (~1,0) gameState gameMap
        | E.KeyLeft  => move (~1,0) gameState gameMap
        | E.KeyS     => move (0,1) gameState gameMap
        | E.KeyDown  => move (0,1) gameState gameMap
        | E.KeyD     => move (1,0) gameState gameMap
        | E.KeyRight => move (1,0) gameState gameMap
        | E.KeySpace => let val (rx, ry) = (Random.range(0, 10) rng - 5,
                                            Random.range(0, 10) rng - 5)
                        in move (rx, ry) gameState gameMap end
        | _ => gameState

end
