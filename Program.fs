open Expecto
open System

type Zelle =
  | Lebt
  | Tot

type Petrischale = Zelle [,]

type Koordinate =
  {
    X : int 
    Y : int
  }
let koordinate (x: int) (y: int): Koordinate =
  { X = x; Y = y }

let ist_legal petri koordinate =
  koordinate.X >= 0 && 
  koordinate.X < Array2D.length1 petri && 
  koordinate.Y >= 0 && 
  koordinate.Y < Array2D.length2 petri

let hole_Zelle (petri : Petrischale) (koordinate : Koordinate) =
  if koordinate |> ist_legal petri then
    Some petri.[koordinate.X, koordinate.Y]
  else 
    None

let petri = array2D [[Tot; Lebt]; [Tot; Tot]]
let zelle = petri.[0,1]

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
  Array2D.mapi (einzelschritt petri) petri

let erstelle breite höhe (lebende_zellen : Koordinate list) =
  let erstelle_zelle x y =
    if lebende_zellen |> List.contains (koordinate x y)
    then Lebt
    else Tot
  Array2D.init breite höhe erstelle_zelle

let zeige_Petrischale petri =
  petri 
  |> Array2D.map (function | Lebt -> "x" | Tot -> "o" )
  |> printfn "%A"

let tests =
  testList "Game of Life" [
    testList "Schritt" [
      test "ohne Nachbarn tot" {
        let petri = array2D[
                    [Tot;Tot;Tot;Tot]
                    [Tot;Lebt;Tot;Tot]
                    [Tot;Tot;Tot;Tot]
                    [Tot;Tot;Tot;Tot]]

        let erwartet = array2D[
                            [Tot;Tot;Tot;Tot]
                            [Tot;Tot;Tot;Tot]
                            [Tot;Tot;Tot;Tot]
                            [Tot;Tot;Tot;Tot]]
                   
        Expect.equal erwartet (schritt petri) "Iterationsfunktion falsch"
      }
      test "ohne Nachbarn tot (2)" {
        let petri = erstelle 4 4 [ koordinate 1 1 ]

        let erwartet = array2D[
                            [Tot;Tot;Tot;Tot]
                            [Tot;Tot;Tot;Tot]
                            [Tot;Tot;Tot;Tot]
                            [Tot;Tot;Tot;Tot]]
                   
        Expect.equal erwartet (schritt petri) "Iterationsfunktion falsch"
      }
      test "2 Nachbarn bleibt am Leben" {
        let petri = array2D[
                    [Tot;Lebt;Tot;Tot]
                    [Lebt;Lebt;Tot;Tot]
                    [Tot;Tot;Tot;Tot]
                    [Tot;Tot;Tot;Tot]]
        let erwartet = array2D[
                            [Lebt;Lebt;Tot;Tot]
                            [Lebt;Lebt;Tot;Tot]
                            [Tot;Tot;Tot;Tot]
                            [Tot;Tot;Tot;Tot]]
                   
        Expect.equal (schritt petri) erwartet "Iterationsfunktion falsch"
      }
      test "2 Nachbarn bleibt am Leben (2)" {
        let petri = erstelle 4 4 [
          koordinate 0 1
          koordinate 1 0
          koordinate 1 1
        ]
        let erwartet = erstelle 4 4 [
          koordinate 0 0
          koordinate 0 1
          koordinate 1 0
          koordinate 1 1
        ]
                   
        Expect.equal (schritt petri) erwartet "Iterationsfunktion falsch"
      }
    ]
    testList "Lebende Nachbarn" [
      test "0" {
        let petri = array2D[[Tot;Tot;Tot;Tot]
                            [Tot;Lebt;Tot;Tot]
                            [Tot;Tot;Tot;Tot]
                            [Tot;Tot;Tot;Tot]]
        let nachbarn = 
          koordinate 1 1
          |> lebende_Nachbarn petri      

        Expect.equal nachbarn 0 "Anzahl inkorrekt"       
      }   

      test "1" {
        let petri = array2D[[Tot;Tot;Tot;Tot]
                            [Tot;Lebt;Tot;Tot]
                            [Tot;Tot;Lebt;Tot]
                            [Tot;Tot;Tot;Tot]]
        let nachbarn = 
          koordinate 1 0
          |> lebende_Nachbarn petri       

        Expect.equal nachbarn 1 "Anzahl inkorrekt"       
      }   
// TODO weitere Cases 3-7
      test "8" {
        let petri = array2D[[Lebt;Lebt;Lebt;Tot]
                            [Lebt;Lebt;Lebt;Tot]
                            [Lebt;Lebt;Lebt;Tot]
                            [Tot;Tot;Tot;Tot]]
        let nachbarn = 
          koordinate 1 1          
          |> lebende_Nachbarn petri      

        Expect.equal nachbarn 8 "Anzahl inkorrekt"       
      }
    ]
    testList "Regeln" [
      // Propertybased testing!
        test "toter mit 3 Nachbarn lebt wieder" {
          let zelle = regeln_anwenden 3 Tot
          Expect.equal zelle Lebt "Zelle sollte leben"
        }
        test "lebender mit 0 Nachbarn stirbt" {
          let zelle = regeln_anwenden 0 Lebt
          Expect.equal zelle Tot "Zelle sollte gestorben sein"
        }
        test "lebender mit 2 Nachbarn bleibt am Leben" {
          let zelle = regeln_anwenden 2 Lebt
          Expect.equal zelle Lebt "Zelle sollte am Leben sein"
        }
        test "lebender mit 3 Nachbarn bleibt am Leben" {
          let zelle = regeln_anwenden 3 Lebt
          Expect.equal zelle Lebt "Zelle sollte am Leben sein"
        }
        test "lebender mit 234789 Nachbarn stirbt" {
          let zelle = regeln_anwenden 234789 Lebt
          Expect.equal zelle Tot "Zelle sollte gestorben sein"
        }
        test "lebender mit -1 Nachbarn stirbt" {
          let zelle = regeln_anwenden -1 Lebt
          Expect.equal zelle Tot "Zelle sollte gestorben sein"
        }
    ]
  ]


[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args tests |> ignore

  let initial =
    erstelle 8 8 [
    koordinate 2 0
    koordinate 2 1
    koordinate 2 2
    koordinate 1 2
    koordinate 0 1
  ]

  let rec nächste petri =
    zeige_Petrischale petri
    Console.ReadLine() |> ignore

    petri 
    |> schritt
    |> nächste


  nächste initial



  0
