// Part of the "Robots" module
module Robots

// Initialize board size
let r = 5
let c = 5

// Initialize robot and element lists
let mutable (robos:Robots.Robot list) = []
let mutable (elems:Robots.BoardElement list) = []

// Initialize BoardDisplay object
let BD = Robots.BoardDisplay (r, c)

// Create the board frame
let BF = Robots.BoardFrame (r, c)

// Create board object
let B = Robots.Board(robos, elems)

// Create robot and other objects
let mutable gg = Robots.Goal (1, 1, "gg")
let AA = Robots.Robot (2, 1, "AA")
let BB = Robots.Robot (4, 5, "BB")
let CC = Robots.Robot (2, 3, "CC")
let HW1 = Robots.HorizontalWall (4, 5, -4)
let HW2 = Robots.HorizontalWall (3, 1, 1)
let HW3 = Robots.HorizontalWall (3, 5, -2)
let HW4 = Robots.HorizontalWall (1, 3, 1)
let HW5 = Robots.HorizontalWall (2, 3, 1)
let HW6 = Robots.HorizontalWall (1, 5, 1)
let VW1 = Robots.VerticalWall (4, 2, -3)
let VW2 = Robots.VerticalWall (3, 1, -2)

// Add robots and elements to respective lists
B.AddRobot AA
B.AddRobot BB
B.AddRobot CC
B.AddElement BF
B.AddElement HW1
B.AddElement HW2
B.AddElement HW3
B.AddElement HW4
B.AddElement HW5
B.AddElement HW6
B.AddElement VW1
B.AddElement VW2


// Main program
[<EntryPoint>]
let main args =
    System.Console.Clear()
    let mutable inputR = 0
    let mutable inputC = 0
    let mutable isValid = false
    printfn "Hello! And welcome to Ricochet Robots!"
    printfn "First you'll have to know that the current board size, is (%A, %A)"
        r c
    printfn ""
    printfn "Now, please input a position for the goal to be placed in
(the location should be within the size of the board):"
    printfn ""
    while not isValid do
        printfn "What do you want the R coordinate of the goal to be?
Default is 1
Recommended for this puzzle is 5:"
        inputR <- 
            try int(System.Console.ReadLine())
            with _ -> 0

        printfn "And what do you want the C coordinate of the goal to be?
Default is 1:
Recommended for this puzzle is 5:"
        inputC <- 
            try int(System.Console.ReadLine())
            with _ -> 0
        
        if (inputR <= r && inputR > 0) && (inputC <= c && inputC > 0) then
            inputR <- inputR
            inputC <- inputC
            isValid <- true
        else
            printfn "Sorry, your input is invalid. Please try again."
            isValid <- false
    
    gg <- Robots.Goal (inputR, inputC, "gg")
    B.AddElement gg
    for elm in B.Elements do
        elm.RenderOn BD
    let newGame = new Robots.Game(B, BD)
    newGame.Play
