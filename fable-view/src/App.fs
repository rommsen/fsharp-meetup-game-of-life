module App

type Zelle =
  | Lebt
  | Tot

type Petrischale = Zelle[][]

type Koordinate =
  {
    X : int 
    Y : int
  }
let koordinate (x: int) (y: int): Koordinate =
  { X = x; Y = y }

let ist_legal petri koordinate =
  koordinate.X >= 0 && 
  koordinate.X < Array.length petri && 
  koordinate.Y >= 0 && 
  koordinate.Y < Array.length petri.[0]

let hole_Zelle (petri : Petrischale) (koordinate : Koordinate) =
  if koordinate |> ist_legal petri then
    Some petri.[koordinate.X].[koordinate.Y]
  else 
    None

type Nachbarn = Zelle []

let nachbarn (petri : Petrischale) {X = x; Y = y} =
  [
    koordinate (x - 1) (y - 1)
    koordinate (x)     (y - 1)
    koordinate (x + 1) (y - 1)
    koordinate (x - 1) (y)
    koordinate (x + 1) (y)
    koordinate (x - 1) (y + 1)
    koordinate (x)     (y + 1)
    koordinate (x + 1) (y + 1)
  ] |> List.choose (hole_Zelle petri)

let lebende_Nachbarn (petri : Petrischale) koordinate =
  nachbarn petri koordinate
  |> List.filter (function | Lebt -> true | Tot -> false)
  |> List.length

let regeln_anwenden anzahl zustand =
  match zustand with
  | Tot when anzahl = 3 -> Lebt
  | Lebt when anzahl < 2 || anzahl > 3 -> Tot
  | _ -> zustand

let private einzelschritt petri x y zelle =
  regeln_anwenden (lebende_Nachbarn petri (koordinate x y)) zelle

let schritt petri : Petrischale =
  let s = einzelschritt petri
  Array.mapi (fun x -> Array.mapi (fun y -> einzelschritt petri x y )) petri

let init_Petri breite höhe initializer =
    [| for i in 1 .. breite -> ([| for j in 1 .. höhe -> initializer i j |]) |]

let erstelle breite höhe (lebende_zellen : Koordinate list) =
  let erstelle_zelle x y =
    if lebende_zellen |> List.contains (koordinate x y)
    then Lebt
    else Tot
  init_Petri breite höhe erstelle_zelle

// --------------------------------------------------------------------------------

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

let window = Browser.Dom.window

// Get our canvas context 
// As we'll see later, myCanvas is mutable hence the use of the mutable keyword
// the unbox keyword allows to make an unsafe cast. Here we assume that getElementById will return an HTMLCanvasElement 
let mutable myCanvas : Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "myCanvas"  // myCanvas is defined in public/index.html

// Get the context
let ctx = myCanvas.getContext_2d()

type Complex = { r : double; i : double }
type Color = { r : int; g : int; b : int; a : int }

let height = 64 
let width = 256

let rnd = System.Random ()
//      erstelle width height [
//     koordinate 2 0
//     koordinate 2 1
//     koordinate 2 2
//     koordinate 1 2
//     koordinate 0 1
// ]

let getCoordColor (x : int, y : int) (petri: Petrischale) : Color =
    let blue = { r = 128; g = 0; b = 255; a = 255}
    let green = { r = 128; g = 255; b = 0; a = 255}
    let yellow = { r = 255; g = 200; b = 40; a = 255}
    let black = { r = 0; g = 30; b = 70; a = 255}

    match petri.[x].[y] with
        | Lebt -> yellow
        | Tot -> black

let mutable petri = init_Petri width height (fun _ _ -> rnd.Next() % 2 |> function | 1 -> Lebt | _ -> Tot)

let showSet () =
    let canvas: Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "myCanvas"
    let ctx = canvas.getContext_2d()

    let img = ctx.createImageData(float width, float height)

    let rec redraw () =
        petri <- schritt petri
        for y = 0 to height-1 do
            for x = 0 to width-1 do
                let index = (x + y * width) * 4
                let color = getCoordColor (x, y) petri
                img.data.[index+0] <- uint8 color.r
                img.data.[index+1] <- uint8 color.g
                img.data.[index+2] <- uint8 color.b
                img.data.[index+3] <- uint8 color.a
        ctx.putImageData(img, 0., 0.)
        JS.setTimeout redraw 0 |> ignore
    redraw ()

showSet ()

