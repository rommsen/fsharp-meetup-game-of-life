module App

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

let height = 512
let width = 512

let getCoordColor (x : int, y : int) : Color =
    let i = (x / 8 * (y % 64 - x)) % 255
    let j = (x * y) % 255
    { r = i; g = j; b = i - j; a = 255}

let showSet() =
    let canvas: Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "myCanvas"
    let ctx = canvas.getContext_2d()

    let img = ctx.createImageData(float width, float height)
    for y = 0 to height-1 do
        for x = 0 to width-1 do
            let index = (x + y * width) * 4
            let color = getCoordColor (x, y)
            img.data.[index+0] <- uint8 color.r
            img.data.[index+1] <- uint8 color.g
            img.data.[index+2] <- uint8 color.b
            img.data.[index+3] <- uint8 color.a
    ctx.putImageData(img, 0., 0.)

showSet()
