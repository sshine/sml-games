(*
 * TODO:
 *  - Display field of view based on actual system clock time
 *  - Event rate limiting (avoid duplicate events after each other)
 *)

(* Why does ML not have this already? *)
infixr $
fun f $ x = f x

local
  fun foldli' f i e [] = e
    | foldli' f i e (x::xs) = foldli' f (i+1) (f (i, x, e)) xs
in
  fun foldli f = foldli' f 0
end

(* Random numbers *)

;load "Random";

val rng = Random.newgen ()

(* Game logic *)

datatype drawable = Grass | Rock | Water | Tree | Door

type PlayerPos = int * int

datatype gamestate = GameState of PlayerPos

fun getPlayerPos (GameState pp) = pp

fun getMapPos (0, 0) ((x::_)::_)  = x
  | getMapPos (x, 0) ((_::xs)::_) = getMapPos (x-1, 0) [xs]
  | getMapPos (x, y) (_::ys)      = getMapPos (x, y-1) ys
  | getMapPos _      _            = Grass

fun addp (x1,y1) (x2,y2) = (x1+x2,y1+y2)

fun move p1 (GameState p2) gameMap =
    let val p' = addp p1 p2 in
      case getMapPos p' gameMap of
          Grass => GameState p'
        | _     => GameState p2
    end

fun distance (x1,y1) (x2,y2) =
    round (Math.sqrt (real (abs (x2 - x1) * abs (y2 - y1))))

val element =
    fn #" " => Grass
     | #"#" => Rock
     | #"~" => Water
     | #"T" => Tree
     | #"D" => Door
     |   _  => Water  (* we're on islands! *)

fun withFile f fname =
    let val fd = TextIO.openIn fname
    in (f (TextIO.inputAll fd) before TextIO.closeIn fd)
    handle Io x => raise Io x
         | exn  => (TextIO.closeIn fd; raise exn)
    end

fun getMap fname =
    let fun isBreak c = (c = #"\r" orelse c = #"\n")
    in withFile (fn s => map (map element o explode) (String.tokens isBreak s)) fname
    end

(* MosGame stuff *)

;load "MosGame";

(* MEDIC! *)
structure M = MosGame
structure E = M.Event
structure D = M.Draw
structure I = M.Image
(*structure C = M.Colors*)

val displayWidth = 640
val displayHeight = 480
val squareSize = 20
val tilesX = displayWidth div squareSize
val tilesY = displayHeight div squareSize


(* Initialize screen *)
val _ = M.init ()
val disp =  M.Display.create_display (displayWidth, displayHeight)


(* Drawing functions *)
fun gridCoords (i, j) = (squareSize*i, squareSize*j)

fun draw fs =
    app (fn f => ignore $ f ()) fs
    before M.Display.flip disp

fun drawbox (i,j) color =
    let val (x,y) = gridCoords (i,j)
        val rect = D.FilledRectangle((x, y), (squareSize, squareSize))
    in D.draw_rectangle disp rect color
    end

fun drawcircle (i,j) color =
    let val r = squareSize div 2
        val (x, y) = addp (r, r) $ gridCoords (i,j)
        val circle = D.FilledCircle ((x,y), r)
    in D.draw_circle disp circle color
    end

fun drawsymb (i, j) symb =
    case symb of
        Grass => drawbox (i,j) M.Green
      | Rock  => drawbox (i,j) M.DarkGray
      | Water => drawbox (i,j) M.Blue
      | Tree  => drawbox (i,j) (M.RGB (0, 140, 0))
      | Door  => drawbox (i,j) (M.RGB (128, 64, 0))

fun sameScreen (i,j) (x,y) =
    i div tilesX = x div tilesX andalso
    j div tilesY = y div tilesY

fun drawMap gameMap gameState _ =
    foldli (fn (j, rows, _) =>
      foldli (fn (i, symb, _) =>
     (* if distance (i,j) (getPlayerPos gameState) < 5 then *)
     (* if Random.range (0, 2) rng <> 0 then *)
        if sameScreen (i, j) (getPlayerPos gameState)
        then drawsymb (i mod tilesX, j mod tilesY) symb
        else ()
      ) () rows) () gameMap

fun drawPlayer (x,y) _ =
    drawcircle (x mod tilesX, y mod tilesY) M.Black

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
        E.KeyQ => (quit (); gameState)
      | E.KeyW => move (0,~1) gameState gameMap
      | E.KeyA => move (~1,0) gameState gameMap
      | E.KeyS => move (0,1) gameState gameMap
      | E.KeyD => move (1,0) gameState gameMap
      | _ => gameState

(* The game loop *)
fun loop gameState gameMap =
    (draw [ drawMap gameMap gameState
          , drawPlayer (getPlayerPos gameState)];
     loop (process gameState gameMap) gameMap)

fun run _ =
    let val jungle = getMap "jungle.txt"
    in loop (GameState (6,6)) jungle
    end

val _ = run ()
