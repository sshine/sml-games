
;load "Random";
;load "MosGame";

local

(* Random number generator *)
val rng = Random.newgen ()
fun rrange (min, max) = Random.range(min, max) rng
fun rtake xs = List.nth (xs, rrange (0, length xs))

(* MosGame *)
structure M = MosGame
structure E = M.Event
structure D = M.Draw
structure I = M.Image

(* Initialization *)
val gridSize = 640
val tileSize = 20

val _ = M.init ()
val disp =  M.Display.create_display (gridSize, gridSize)

(* Drawing boxes *)
val colors = [ M.Red, M.Green, M.Blue, M.Black ]

fun drawbox NONE =
    let val x = rrange(0, gridSize div tileSize) * tileSize
        val y = rrange(0, gridSize div tileSize) * tileSize
    in drawbox (SOME (x, y)) end
  | drawbox (SOME (x, y)) =
    let val size = rrange (0, 4) + 1
        val padding = 5
        val rect = D.FilledRectangle((x+padding, y+padding),
                                     (tileSize * size - padding, tileSize * size - padding))
        val color = rtake colors
    in D.draw_rectangle disp rect color
    end

fun loop _ =
    (drawbox NONE;
     M.Display.flip disp;
     OS.Process.sleep (Time.fromMilliseconds (rrange (10, 250)));
     loop ())
in
  val _ = loop ()
end
