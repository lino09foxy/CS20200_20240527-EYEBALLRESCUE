module EyeballRescue.Program

open System

type Tile =
  | P
  | B
  | E
  | W
  | Empty
  | T

type GameState = {
  Grid: Tile list list
  Size: int
  PlayerR: int
  PlayerC: int
  HasBottle: bool
  Difficulty: int
}

// Easy: 5x5
let easyMap = [
  [ P; Empty; W; T; E ];
  [ T; Empty; W; Empty; Empty ];
  [ W; Empty; Empty; Empty; W ];
  [ Empty; Empty; W; Empty; T ];
  [ B; T; W; Empty; Empty ]
]

// Normal: 7x7 
let normalMap = [
  [ P; Empty; W; Empty; Empty; T; Empty ];
  [ W; Empty; W; Empty; W; W; Empty ];
  [ Empty; Empty; Empty; Empty; Empty; W; Empty ];
  [ Empty; W; W; W; Empty; Empty; Empty ];
  [ Empty; Empty; Empty; W; Empty; W; W ];
  [ W; W; Empty; W; Empty; B; Empty ];
  [ E; Empty; Empty; T; Empty; W; T ]
]

// Hard: 9x9 
let hardMap = [
  [ P; T; Empty; Empty; Empty; T; Empty; Empty; Empty ];
  [ Empty; W; Empty; W; Empty; T; Empty; W; Empty ];
  [ Empty; Empty; Empty; T; Empty; Empty; Empty; T; Empty ];
  [ T; W; W; W; T; W; W; W; Empty ];
  [ T; W; T; W; T; W; Empty; Empty; Empty ];
  [ W; T; W; T; W; T; Empty; W; T ];
  [ T; W; Empty; Empty; Empty; W; Empty; B; Empty ];
  [ W; T; Empty; W; Empty; T; W; W; Empty ];
  [ E; Empty; Empty; T; Empty; Empty; Empty; Empty; Empty ]
]

let initGame a =
  match a with
  | 1 -> { Grid = easyMap; Size = 5; PlayerR = 0; PlayerC = 0; HasBottle = false; Difficulty = 1 }
  | 2 -> { Grid = normalMap; Size = 7; PlayerR = 0; PlayerC = 0; HasBottle = false; Difficulty = 2 }
  | 3 -> { Grid = hardMap; Size = 9; PlayerR = 0; PlayerC = 0; HasBottle = false; Difficulty = 3 }
  | _ -> failwith "Invalid difficulty"

let printMap a =
  let b = a.Grid
  let c = a.Size
  
  for i = 0 to c - 1 do
    for j = 0 to c - 1 do
      let d = 
        if a.PlayerR = i && a.PlayerC = j then "P "
        else
          let e = List.item j (List.item i b)
          match e with
          | W -> "# "
          | E -> "E "
          | B -> if a.HasBottle then ". " else "B "
          | _ -> ". " 
      Console.Write (d)
    
    let f = List.item i b
    let rec countTraps g =
      match g with
      | [] -> 0
      | h :: k -> (if h = T then 1 else 0) + countTraps k
    Console.WriteLine ("row traps: {0}", countTraps f)
  
  Console.WriteLine ("col traps:")
  for j = 0 to c - 1 do
    let rec countColTraps m n =
      match m with
      | [] -> 0
      | p :: q -> (if List.item n p = T then 1 else 0) + countColTraps q n
    Console.Write ("{0} ", countColTraps b j)
  Console.WriteLine ()

let rec promptRestart a =
  Console.WriteLine ("Restart? (Y/N)")
  let line = Console.ReadLine () 
  let b = line.Trim().ToUpper()
  match b with
  | "Y" -> 
    let c = initGame a.Difficulty
    gameLoop c
  | "N" -> ()
  | _ -> promptRestart a

and gameLoop a =
  printMap a
  Console.Write ("Enter move (W/A/S/D): ")
  let line = Console.ReadLine ()
  let b = line.Trim().ToUpper()
  
  let c, d = 
    match b with
    | "W" -> a.PlayerR - 1, a.PlayerC
    | "S" -> a.PlayerR + 1, a.PlayerC
    | "A" -> a.PlayerR, a.PlayerC - 1
    | "D" -> a.PlayerR, a.PlayerC + 1
    | _ -> a.PlayerR, a.PlayerC
  
  if b <> "W" && b <> "A" && b <> "S" && b <> "D" then
    Console.WriteLine ("Invalid movement command.")
    gameLoop a
  elif c < 0 || c >= a.Size || d < 0 || d >= a.Size then
    Console.WriteLine ("Error: Cannot move outside the map.")
    gameLoop a
  else
    let e = List.item d (List.item c a.Grid)
    match e with
    | W ->
      Console.WriteLine ("Error: Cannot move into a wall.")
      gameLoop a
    | T ->
      let f = { a with PlayerR = c; PlayerC = d }
      printMap f 
      Console.WriteLine ("You fell into the Hunter's hidden trap. Game over.")
      promptRestart a
    | E ->
      let f = { a with PlayerR = c; PlayerC = d }
      if a.HasBottle then
        printMap f 
        Console.WriteLine ("You escaped with the eyeballs. You win!")
      else
        Console.WriteLine ("You cannot escape without the captured eyeballs.")
        gameLoop f
    | B ->
      if not a.HasBottle then
        Console.WriteLine ("You found the bottle of captured eyeballs!")
      let f = { a with PlayerR = c; PlayerC = d; HasBottle = true }
      gameLoop f
    | _ ->
      let f = { a with PlayerR = c; PlayerC = d }
      gameLoop f

let rec start () =
  Console.WriteLine ("Eyeball Rescue")
  Console.WriteLine ("Kim eyeball, who had been living happily alongside the Eyeballs at KAIST! One day, a mysterious hunter appeared, captured the Eyeballs, sealed them in a jar, and locked them away in a dungeon. Kim eyeball sneaks into the dungeon to rescue the captured Eyeballs! Will Kim eyeball be able to safely rescue the Eyeballs from the dungeon, which is filled with hidden traps?")
  Console.WriteLine ("Select difficulty:")
  Console.WriteLine ("1. Easy (5 x 5)")
  Console.WriteLine ("2. Normal (7 x 7)")
  Console.WriteLine ("3. Hard (9 x 9)")
  Console.Write ("Enter difficulty: ")
  let line = Console.ReadLine () 
  let a = line.Trim()
  match a with
  | "1" -> gameLoop (initGame 1)
  | "2" -> gameLoop (initGame 2)
  | "3" -> gameLoop (initGame 3)
  | _ -> 
    Console.WriteLine ("Invalid difficulty. Please try again.")
    start ()

[<EntryPoint>]
let main _args =
  Console.OutputEncoding <- Text.Encoding.Unicode
  start ()
  0
