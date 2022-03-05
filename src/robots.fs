// Part of the "Robots" module
module Robots


///<summary>Class to display a board to play on.</summary>
///<param name = "rows">The amount of rows on the playing board.</param>
///<param name = "cols">The amount of columns on the playing board.</param>
type BoardDisplay (rows:int, cols:int) =
    // Initialize class values
    let r = rows
    let c = cols
    let board = Array2D.init (rows*2+1) (cols*2+1) (fun i j -> 
        if (((i % 2) = 0) && ((j % 2) = 0)) then "+"
        else if ((i = 0) || (i = rows*2)) then "--"
        else if ((j = 0) || (j = cols*2)) then "|"
        else if ((i % 2 = 1 || i % 2 = 0) && j % 2 = 1) then "  "
        else " ")

    ///<summary>Get the BoardDisplay's row value.</summary>
    ///<returns>The BoardDisplay's row value.</returns>
    member __.R = r

    ///<summary>Get the BoardDisplay's column value.</summary>
    ///<returns>The BoardDisplay's column value.</returns>
    member __.C = c

    ///<summary>Overrides board index with content value.</summary>
    ///<param name = "row">Row index.</param>
    ///<param name = "col">Column index.</param>
    ///<param name = "cont">Content to override with.</param>
    member __.Set (row:int) (col:int) (cont:string) =
        let r = row*2 - 1
        let c = col*2 - 1
        board.[r, c] <- cont.[..1]

    ///<summary>Sets a horizontal/bottom wall on the board.</summary>
    ///<param name = "row">Row value to place wall on.</param>
    ///<param name = "col">Column value to place wall on.</param>
    member __.SetBottomWall (row:int) (col:int) =
        let r = row*2
        let c = col*2 - 1
        if r > __.R*2 || r <= 0 || c > __.C*2 - 1 || c <= 0 then
            board.[__.R*2, __.C*2 - 1] <- "--"
        else board.[r, c] <- "--"
        
    ///<summary>Sets a vertical/right wall on the board.</summary>
    ///<param name = "row">Row value to place wall on.</param>
    ///<param name = "col">Column value to place wall on.</param>
    member __.SetRightWall (row:int) (col:int) =
        let r = row*2 - 1
        let c = col*2
        if r > __.R*2 - 1 || r <= 0 || c > __.C*2 || c <= 0 then
            board.[__.R*2 - 1, __.C*2] <- "|"
        else board.[r, c] <- "|"
    

    ///<summary>Prints the board nicely in the terminal.</summary>
    member __.Show =
        for i in 0 .. r*2 do
            for j in 0 .. c*2 do
                if j = c*2 then printf "%s\n" board.[i, j]
                else printf "%s" board.[i, j]


// Define various types to use.
type Direction = North | South | East | West
type Position = int*int
type Action =
    | Stop of Position
    | Continue of Direction * Position // Not used as we don't have a teleporter
    | Ignore


///<summary>Abstract class with member functions to inherit.</summary>
[<AbstractClass>]
type BoardElement() =
    abstract member RenderOn : BoardDisplay -> unit
    abstract member Interact : Robot -> Direction -> Action
    default __.Interact _ _ = Ignore
    abstract member GameOver : Robot list -> bool
    default __.GameOver _ = false


///<summary>Class to represent a Robot object.</summary>
///<param name = "row">Row position.</param>
///<param name = "col">Column position.</param>
///<param name = "name">Name of robot.</param>
and Robot(row:int, col:int, name:string) =
    // Inherit BoardElement class member functions.
    inherit BoardElement()

    // Initialize class values.
    let mutable r = row
    let mutable c = col
    let n = name
    let rFix = row*2 - 1
    let cFix = row*2 - 1

    ///<summary>Get the position of the robot.</summary>
    ///<returns>A tuple of integers represnting the robot's position.</returns>
    member __.Position : Position = (r, c)

    ///<summary>Get the row position value of the robot.</summary>
    ///<returns>The row position of the robot.</returns>
    member __.R = r

    ///<summary>Get the column position value of the robot</summary>
    ///<returns>The column position of the robot.</returns>
    member __.C = c
    
    ///<summary>Sets the robot's position.</summary>
    ///<param name = "newR">New row value to set on robot's position.</param>
    ///<param name = "newC">New column value to set on robot's psotion.</param>
    member __.SetPosition (newR:int, newC:int) =
        r <- newR
        c <- newC
    
    ///<summary>Render the robot onto the board.</summary>
    ///<param name = "BD">BoardDisplay object to render robot onto.</param>
    override __.RenderOn (BD:BoardDisplay) = BD.Set r c n

    ///<summary>Return the name of the robot.</summary>
    ///<returns>The name of the robot.</returns>
    member val Name = n
    
    ///<summary>Move one step in a direction.</summary>
    ///<param name = "dir">The direction to move in.</param>
    member robot.Step (dir:Direction) =
        match dir with
            | North -> r <- r - 1
            | South -> r <- r + 1
            | East -> c <- c + 1
            | West -> c <- c - 1

    ///<summary>Stop a robot if it's about to collide with it.</summary>
    ///<param name = "other">Robot to check for collision.</param>
    ///<param name = "dir">Direction the robot is moving.</param>
    ///<returns>The position to stop in, or Ignore.</returns>
    override __.Interact (other:Robot) (dir:Direction) =
        let (r1, c1) = other.Position
        if r1 - 1 = r && c1 = c && dir = North then Stop(other.Position)
        else if r1 + 1 = r && c1 = c && dir = South then Stop(other.Position)
        else if r1 = r && c1 - 1 = c && dir = West then Stop(other.Position)
        else if r1 = r && c1 + 1 = c && dir = East then Stop(other.Position)
        else Ignore


///<summary>Represent a Goal object.</summary>
///<param name = "row">Row position value.</param>
///<param name = "col">Column position value.</param>
///<param name = "name">Name of the Goal object.</param>
type Goal(row:int, col:int, name:string) =
    // Inherit BoardElement class member functions
    inherit BoardElement()

    // Initialize class values
    let r = row
    let c = col
    let n = name

    ///<summary>Get the position of the Goal object.</summary>
    ///<returns>The position of the Goal object.</returns>
    member __.Position = (r, c)

    ///<summary>Render the Goal object onto the Board.</summary>
    ///<param name = "BD">BoawrdDisplay element to render the Goal onto.</param>
    override __.RenderOn (BD:BoardDisplay) = BD.Set r c n

    ///<summary>Check if any robot is placed on a Goal object.</summary>
    ///<param name = "robos">A list of robots.</param>
    ///<returns>A boolean relating to iff a robot is on the Goal.</returns>
    override __.GameOver (robos:Robot list) : bool =
        Array.exists (fun (r:Robot) -> r.Position = __.Position) //->
            (Array.ofList robos) //End Line


///<summary>BoardFrame object to represent the outer walls on a board.</summary>
///<param name = "row">Row size value of the board.</param>
///<param name = "col">Column size value of the board.</param>
type BoardFrame(row:int, col:int) =
    // Inherit BoardElement class member functions
    inherit BoardElement()

    // Initialize class values
    let r = row
    let c = col

    ///<summary>Renders the BoardFrame object onto the board.</summary>
    ///<param name = "BD">BoardDisplay to render the BoardFrame onto.</param>
    override __.RenderOn (BD:BoardDisplay) = ()

    ///<summary>Stop a robot if it's about to collide with it.</summary>
    ///<param name = "other">Robot to check for collision.</param>
    ///<param name = "dir">Direction robot is moving.</param>
    ///<returns>The position to stop in, or Ignore.</returns>
    override __.Interact (other:Robot) (dir:Direction) =
        let (r1, c1) = other.Position
        if r1 - 1 = 0 && dir = North then Stop(other.Position)
        else if r1 = r && dir = South then Stop(other.Position)
        else if c1 - 1 = 0 && dir = West then Stop(other.Position)
        else if c1 = c && dir = East then Stop(other.Position)
        else Ignore


///<summary>Represent a VerticalWall object.</summary>
///<param name = "row">Row position of wall.</param>
///<param name = "col">Column position of wall.</param>
///<param name = "n">Length of wall.</param>
type VerticalWall(row:int, col:int, n:int) =
    // Inherit BoardElement class member functions
    inherit BoardElement()

    // Initialize class values
    let mutable r = row
    let c = col
    let mutable len =
        if n > 0 then n
        else
            r <- r + n + 1
            n*(-1)
    
    ///<summary>Get the position of the wall.</summary>
    ///<returns>The position of the wall.</returns>
    member __.Position = (r, c)

    ///<summary>Render the VerticalWall object onto the board.</summary>
    ///<param name = "BD">BoardDisplay element to render the wall onto.</param>
    override __.RenderOn (BD:BoardDisplay) =
        let mutable i = 0
        while len + r > BD.R + 1 do
            len <- len - 1
        while i < len do
            BD.SetRightWall (r + i) c
            i <- i + 1

        
    ///<summary>Stop a robot if it's about to collide with it.</summary>
    ///<param name = "other">Robot to check for collision.</param>
    ///<param name = "dir">Direction robot is moving.</param>
    ///<returns>The position to stop in, or Ignore.</returns>
    override __.Interact (other:Robot) (dir:Direction) =
        match len with
            | x when x = 0 -> Ignore
            | x when x < 0 ->
                if other.R <= r && other.R >= r + len - 1 && other.C = c &&
                    dir = East then Stop(other.Position)
                else if other.R <= r && other.R >= r + len-1 && other.C-1 = c &&
                    dir = West then Stop(other.Position)
                else Ignore
            | x when x > 0 ->
                if other.R >= r && other.R <= r + len - 1 && other.C = c &&
                    dir = East then Stop(other.Position)
                else if other.R >= r && other.R <= r + len-1 && other.C-1 = c &&
                    dir = West then Stop(other.Position)
                else Ignore


///<summary>Represent a HorizontalWall object.</summary>
///<param name = "row">Row position of wall.</param>
///<param name = "col">Column position of wall.</param>
///<param name = "n">Length of wall.</param>
type HorizontalWall (row:int, col:int, n:int) =
    // Inherit BoardElement class member functions
    inherit BoardElement()

    // Initialize class values
    let r = row
    let mutable c = col
    let mutable len = 
        if n > 0 then n
        else
            c <- c + n + 1
            n*(-1)

    ///<summary>Get the position of the wall.</summary>
    ///<returns>The position of the wall.</returns>
    member __.Position = (r, c)

    ///<summary>Render the VerticalWall object onto the board.</summary>
    ///<param name = "BD">BoardDisplay element to render the wall onto.</param>
    override __.RenderOn (BD:BoardDisplay) =
        let mutable i = 0
        while len + c > BD.C + 1 do
            len <- len - 1
        while i < len do
            BD.SetBottomWall r (c + i)
            i <- i + 1
        
    ///<summary>Stop a robot if it's about to collide with it.</summary>
    ///<param name = "other">Robot to check for collision.</param>
    ///<param name = "dir">Direction robot is moving.</param>
    ///<returns>The position to stop in, or Ignore.</returns>
    override __.Interact (other:Robot) (dir:Direction) =
        match len with
            | x when x = 0 -> Ignore
            | x when x < 0 ->
                if other.R = r && other.C <= c && other.C >= c + len - 1 &&
                    dir = South then Stop(other.Position)
                else if other.R-1 = r && other.C <= c && other.C >= c + len-1 &&
                    dir = North then Stop(other.Position)
                else Ignore
            | x when x > 0 ->
                if other.R = r && other.C >= c && other.C <= c + len - 1 &&
                    dir = South then Stop(other.Position)
                else if other.R-1 = r && other.C >= c && other.C <= c + len-1 &&
                    dir = North then Stop(other.Position)
                else Ignore


///<summary>Board object to represent the playing board.</summary>
///<param name = "roboLst">List of robots on the board.</param>
///<param name = "elemLst">List of elements on the board.</param>
///<returns>New instance of a Board obejct.</returns>
type Board (roboLst:Robot list, elemLst:BoardElement list) =
    // Initialize class values
    let mutable robos = roboLst
    let mutable elems = elemLst
    let mutable elmList =
        for r in robos do
            let (rob:BoardElement) = upcast r
            if (not (List.contains rob elems)) then elems <- rob::elems
            else ()
        elems

    ///<summary>Add a robot to the list of robots on the board.</summary>
    ///<param name = "robot">Robot to add to the list of robots.</param>
    member __.AddRobot (robot:Robot) =
        robos <- robot::robos
        elmList <- (upcast robot)::elmList

    ///<summary>Add an element to the list of elements on the board.</summary>
    ///<param name = "element">Element to add to the list of elements.</param>
    member __.AddElement (element:BoardElement) = elmList <- element::elmList

    ///<summary>Get elements on the board.</summary>
    ///<returns>Elements on the board.</returns>
    member __.Elements = elmList

    ///<summary>Get the robots on the board.</summary>
    ///<returns>Robots on the board.</returns>
    member __.Robots = robos

    ///<summary>Move a robot in a certain direction.</summary>
    ///<param name = "robot">Robot to move</param>
    ///<param name = "dir">Direction to move the robot</param>
    member __.Move (robot:Robot) (dir:Direction) =
        let mutable isMoving = true
        while isMoving do
            let tmpLst = elmList |> List.map (fun (elm:BoardElement) -> //->
                elm.Interact robot dir) //End Line
            
            match ((List.filter (fun (act:Action) -> act <> Ignore)) //->
                tmpLst).Length with //End Line
            | 0 ->
                robot.Step dir
                isMoving <- true
            | 1 ->
                match List.find (fun (act:Action) -> act <> Ignore) tmpLst with
                | Stop (pos) ->
                    robot.SetPosition pos
                    isMoving <- false
                | _ -> () 
            | _ ->
                for elm in tmpLst do
                    match elm with
                    | Stop (pos) ->
                        robot.SetPosition pos
                        isMoving <- false
                    | _ -> ()


///<summary>Re-render the board, and check if the game is over.</summary>
///<param name = "board">The board object of the game.</param>
///<param name = "BD">BoardDisplay object to check render objects.</param>
///<returns>Boolean corresponding to, if the game is over or not.</returns>
let renderBoard (board:Board) (BD:BoardDisplay) : bool = 
    let mutable isGameover = false
    for i in 1..BD.R do
        for j in 1..BD.C do
            BD.Set i j "  "
    for elm in board.Elements do
        if not (elm.GameOver (board.Robots)) then elm.RenderOn BD
        else
            isGameover <- true
            ()
    BD.Show
    isGameover
    //Function created in conjunction with another group.


///<summary>Game object representing a game.</summary>
///<param name = "board">Board object to start the game with.</param>
///<param name = "display">BoardDisplay object to start the game with.</param>
type Game (board:Board, display:BoardDisplay) =
    // Initialize class values
    let mutable B = board
    let mutable BD = display

    ///<summary>Play a game.</summary>
    ///<returns>The amount of moves used.</returns>
    member __.Play : int =
        let mutable isGameover = false
        let mutable moves = 0
        let mutable isMoving = true

        ///<summary>Pick a robot to use in the game.</summary>
        ///<returns>A slected robot.</returns>
        let chooseRobot() = 
            let str = B.Robots |> List.fold (fun i (robo:Robot) -> 
                                    robo.Name + " - (" + string(B.Robots |> 
                                            List.findIndex (fun x -> 
                                            x = robo)) + "), " + i ) ""
            printfn "Select a robot by typing the number shown in the \n
parenthesis next to it's name, and pressing enter:"
            printfn "\n%s" str.[0..str.Length-3]

            let mutable input = -1
            let mutable isValid = false
            while not isValid do
                input <- 
                    try int(System.Console.ReadLine())
                    with _ -> -1
                    
                match input with
                | i when i < B.Robots.Length && i > -1 -> 
                    input <- i
                    isValid <- true
                | _ ->
                    printfn "Sorry, your input is invalid. Please try again."
                    isValid <- false
            B.Robots.[input]
            //Function created in conjunction with another group.

        isGameover <- renderBoard B BD
        System.Console.Clear()

        while not isGameover do
            System.Console.Clear()
            printfn "Moves used: %A" moves
            isGameover <- renderBoard B BD

            let chosenRobot = chooseRobot()
            renderBoard B BD 

            System.Console.Clear()
            printfn "Moves used: %A" moves
            isGameover <- renderBoard B BD

            printfn "Robot %s has been selected" chosenRobot.Name
            printfn "Move the robot by using the arrow keys. "
            printfn "Stop by hitting enter"   
            isMoving <- true

            ///<summary>Move the chosen robot in a specific direction.</summary>
            ///<param name = "dir">Direction to move the chosen robot.</param>
            let moveRobot (dir:Direction) =
                moves <- moves + 1
                B.Move chosenRobot dir
                System.Console.Clear()
                printfn "Moves used: %A" moves
                isGameover <- renderBoard B BD
                printfn "Robot %s has been selected" chosenRobot.Name
                printfn "Move the robot by using the arrow keys. " 
                printfn "Stop by hitting enter"
                if isGameover then isMoving <- false else isMoving <- true

            while isMoving do
                let arrowKey = System.Console.ReadKey(true)
                match arrowKey.Key with
                | System.ConsoleKey.UpArrow -> 
                    System.Console.ForegroundColor <- System.ConsoleColor.Blue
                    printfn "^"
                    moveRobot North
                | System.ConsoleKey.DownArrow -> 
                    System.Console.ForegroundColor <- System.ConsoleColor.Red
                    printfn "V"
                    moveRobot South
                | System.ConsoleKey.RightArrow -> 
                    System.Console.ForegroundColor <- System.ConsoleColor.Green
                    printfn ">"
                    moveRobot East
                | System.ConsoleKey.LeftArrow -> 
                    System.Console.ForegroundColor <- System.ConsoleColor.Yellow
                    printfn "<"
                    moveRobot West
                | System.ConsoleKey.Enter -> 
                    System.Console.ForegroundColor <- System.ConsoleColor.White
                    isMoving <- false 
                    isGameover <- renderBoard B BD
                | _ ->
                    isMoving <- false 
                    isGameover <- renderBoard B BD
        
        ///<summary>The ending screen to be shown to the player.</summary>
        let endScr() = 
            System.Console.Clear()
            printfn "Well done!"
            printfn "You reached the goal in only %A moves!" moves
            renderBoard B BD 
            System.Threading.Thread.Sleep(100)

        for i in 0..1 do
            System.Console.ForegroundColor <- System.ConsoleColor.Blue
            endScr()
            System.Console.ForegroundColor <- System.ConsoleColor.Red
            endScr()
            System.Console.ForegroundColor <- System.ConsoleColor.Green
            endScr()
            System.Console.ForegroundColor <- System.ConsoleColor.Yellow
            endScr()
            System.Console.ForegroundColor <- System.ConsoleColor.Cyan
            endScr()
            System.Console.ForegroundColor <- System.ConsoleColor.DarkRed
            endScr()
            System.Console.ForegroundColor <- System.ConsoleColor.DarkBlue
            endScr()
            System.Console.ForegroundColor <- System.ConsoleColor.DarkYellow
            endScr()
            System.Console.ForegroundColor <- System.ConsoleColor.DarkGreen
            endScr()
            System.Console.ForegroundColor <- System.ConsoleColor.Green
            endScr()
        moves
