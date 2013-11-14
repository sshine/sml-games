;load "MosGame"; (* Until we use Moscow ML structure-mode *)

infixr $
fun f $ x = f x

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

       and GameState = GameState of { player  : Ship * coordinate
                                    , pirates : (Pirate * coordinate) list }

  (* Initial game: 15 men, a cannon and 5 barrels of coffee. *)
  val coffee = Stuff { name = "coffee", worth = (10,30) }

  val me = Ship { money   = 100
                , cannons = 1
                , men     = 15
                , cargo   = [(5, coffee)] }

  (* And the only other pirate is John, who incidentally wants to buy coffee *)
  val john = Pirate { name = "John the Pirate"
                    , ship = Ship { money   = 2000
                                  , cannons = 1
                                  , men     = 10
                                  , cargo   = [(2, coffee)] }
                    , interests = [coffee] }

  fun initialGame () = GameState { player  = (me, (100,100))
                                 , pirates = [(john, (400,400))] }

end


(****************************************************************************)
(****************************************************************************)
(****************************************************************************)
structure M = MosGame
structure E = M.Event
structure D = M.Draw
structure I = M.Image
(*structure C = M.Colors*)

structure Graphics =
struct

  val displayWidth = 960
  val displayHeight = 480
  val scaleFactor = 20 (* when drawing geometric figures, how big should they be *)

  val _ = M.init ()
  val disp =  M.Display.create_display (displayWidth, displayHeight)

  fun draw fs =
      app (fn f => ignore $ f ()) fs
      before M.Display.flip disp


  (* Ahh, the sea... *)
  val sea_gfx = D.FilledRectangle ((0,0), (displayWidth-1, displayHeight-1))
  fun drawSea gameState () = D.draw_rectangle disp sea_gfx M.Blue

  (* Ohh, a ship!!! *)
  val ship_gfx = M.Image.load "myship.png"
  fun drawPlayer (Model.GameState gameState) () =
      let val shipPos = #2 (#player gameState)
      in M.Surface.blit ship_gfx M.Surface.Full disp shipPos end

(* fun draw () = ( *)
(*   M.Surface.blit spaceBg M.Surface.Full disp (0,0); *)
(*   map (fn (bx, by, dx, dy) => *)
(*         M.Surface.blit bulletSprite M.Surface.Full *)
(*                        disp (floor(bx)-8, floor(by)-8)) (!bullets); *)
(*   M.Surface.blit spaceShipSprite (spaceShipRot (!shipRot)) disp (!shipPos); *)
(*   M.Display.flip disp *)
(* ) *)

end


structure Main =
struct

  (* Start a game *)
  fun run () =
      let val gameState = Model.initialGame ()
      in loop gameState end

  (* Game loop *)
  and loop gameState =
      (Graphics.draw [ Graphics.drawSea    gameState
                     , Graphics.drawPlayer gameState ];
       loop (process gameState))

  (* Event handling *)
  and process gameState =
      case E.poll () of
          NONE => gameState
        | SOME E.QuitEvent               => (quit (); gameState)
        | SOME (E.KeyboardEvent data)    => handleKey data gameState
        | SOME (E.MouseButtonEvent data) => handleMouse data gameState
        | SOME _                         => gameState

  and handleKey (state, symb, modifiers) gameState =
      if state <> E.KeyPressed then gameState else
      case symb of
          E.KeyQ     => (quit (); gameState)
        | E.KeySpace => (print "Yeah.\n"; gameState)
        | _ => gameState

  and handleMouse (keystate, mousebutton, (x,y)) gameState =
      (print ("(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ")\n"); gameState)

end
