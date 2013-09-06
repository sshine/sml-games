
(* Why does ML not have this already? *)
infixr $
fun f $ x = f x

(* Random numbers *)

;load "Random";

val rng = Random.newgen ()

(* Game logic *)

datatype drawable = Grass | Rock | Water | Tree | Door

type PlayerPos = int * int

datatype gamestate = GameState of PlayerPos

fun getPlayerPos (GameState pp) = pp

fun addp (x1,y1) (x2,y2) = (x1+x2,y1+y2)

fun move p1 (GameState p2) = GameState (addp p1 p2)

val element =
    fn #" " => Grass
     | #"#" => Rock
     | #"~" => Water
     | #"T" => Tree
     | #"D" => Door

fun withFile f fname =
    let val fd = TextIO.openIn fname
    in (f (TextIO.inputAll fd) before TextIO.closeIn fd)
    handle Io x => raise Io x
         | exn  => (TextIO.closeIn fd; raise exn)
    end

fun getmap fname =
    let fun isBreak c = (c = #"\r" orelse c = #"\n")
    in withFile (fn s => map (map element o explode) (String.tokens isBreak s)) fname
    end

local
  fun foldli' f i e [] = e
    | foldli' f i e (x::xs) = foldli' f (i+1) (f (i, x, e)) xs
in
  fun foldli f = foldli' f 0
end

(* MosGame stuff *)

;load "MosGame";

(* MEDIC! *)
structure M = MosGame
structure E = M.Event
structure D = M.Draw
structure I = M.Image
(*structure C = M.Colors*)

(* Initialize screen *)
val _ = M.init ()
val disp =  M.Display.create_display (640, 480);

(* Drawing functions *)
val squareSize = 20

fun gridCoords (i, j) = (squareSize*i, squareSize*j)

fun draw fs =
    app (fn f => ignore $ f ()) fs
    before M.Display.flip disp

fun drawbox (i,j) color =
    let val (x,y) = gridCoords (i,j)
        val rect = D.FilledRectangle((x, y), (squareSize, squareSize))
    in D.draw_rectangle disp rect color
    end

fun drawsymb (i, j) symb =
    case symb of
        Grass => drawbox (i,j) M.Green
      | Rock  => drawbox (i,j) M.DarkGray
      | Water => drawbox (i,j) M.Blue
      | Tree  => drawbox (i,j) (M.RGB (0, 140, 0))
      | Door  => drawbox (i,j) (M.RGB (128, 64, 0))

fun drawMap m _ =
    foldli (fn (j, rows, _) =>
      foldli (fn (i, symb, _) =>
        drawsymb (i, j) symb) () rows) () m

fun drawPlayer (x,y) _ =
    drawbox (x,y) M.Black

(* Game loop *)
fun process gameState =
    case E.poll () of
        NONE => gameState
      | SOME E.QuitEvent => (quit (); gameState)
      | SOME (E.KeyboardEvent data) => handleKey gameState data
      | SOME _ => gameState

and handleKey gameState (state, symb, modifiers) =
    case symb of
        E.KeyQ => (quit (); gameState)
      | E.KeyW => move (0,~1) gameState
      | E.KeyA => move (~1,0) gameState
      | E.KeyS => move (0,1) gameState
      | E.KeyD => move (1,0) gameState
      | _ => gameState

(*******************)

fun loop gameState gameMap =
    (draw [ drawPlayer (getPlayerPos gameState)
          , drawMap gameMap
          , drawPlayer (getPlayerPos gameState)];
     loop (process gameState) gameMap)

fun run _ =
    let val jungle = getmap "jungle.txt"
    in loop (GameState (6,6)) jungle
    end

val _ = run ()
