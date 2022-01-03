open System

// Converts the choice of player n to an X, O or a space if it is neither
let PlayerToString n =
    match n with
    | 1 -> "X"
    | 2 -> "O"
    | _ -> " "

// Prints the matrix to the console
let PrintMatrix (matrix: list<int>)  =
    let stringMatrix = [ for x in matrix do (PlayerToString x) ]
    printfn " %s | %s | %s " stringMatrix[2] stringMatrix[5] stringMatrix[8]
    printfn "-----------"
    printfn " %s | %s | %s " stringMatrix[1] stringMatrix[4] stringMatrix[7]
    printfn "-----------"
    printfn " %s | %s | %s " stringMatrix[0] stringMatrix[3] stringMatrix[6]
    ()

// Fetches a valid user input from the command line
let rec GetUserInput () =
    let input = Console.ReadLine ()
    let x = int input in
    match x with
    | 1 | 2 | 3 -> x
    | _ ->
        printfn "Invalid input. Please try again."
        GetUserInput ()

// Converts inputted coordinates to an index in the list of points in the matrix
let CoordsToIndex x y =
    match x with
    | 1 -> y - 1
    | 2 -> y + 2
    | 3 -> y + 5
    | _ -> -1

// Checks if the point in the matrix has already been chosen
let HasBeenChosen index (matrix: list<int>) =
    match matrix[index] with
    | 0 -> false
    | _ -> true

// Gets a valid index in the list of points in the matrix
let rec GetIndex matrix =
    printfn "On what x coordinate is the square which you wish to choose"
    let x = GetUserInput ()
    printfn "On what y coordinate is the square which you wish to choose"
    let y = GetUserInput ()
    let index = CoordsToIndex x y
    match (HasBeenChosen index matrix) with
    | true -> 
        printfn "That point has already been chosen. Please choose one that hasn't been chosen yet."
        GetIndex matrix
    | false -> index

// Checks if num fills up a whole row or column
let CheckNonDiagonals num (matrix: list<int>) =
    let nonDiagonals = [ 
        for i in 0..2 do
            let multi = i * 3
            if matrix[multi] = num && matrix[multi+1] = num && matrix[multi+2] = num then yield true
            elif matrix[i] = num && matrix[i+3] = num && matrix[i+6] = num then yield true
            else yield false
    ] in
    List.contains true nonDiagonals

// Checks if num fills up a whole diagonal of length 3
let CheckDiagonals num (matrix: list<int>) =
    if matrix[2] = num && matrix[4] = num && matrix[6] = num then true
    elif matrix[0] = num && matrix[4] = num && matrix[8] = num then true
    else false

let HasWon player (matrix: list<int>) =
    CheckNonDiagonals player matrix || CheckDiagonals player matrix

let ChangeIndex value index matrix =
    List.mapi (fun i x -> if i = index then value else x) matrix

let PrintWin player matrix =
    Console.Clear ()
    printfn "Congratulations to player %i who has won!" player
    PrintMatrix matrix

let PrintDraw () =
    printfn "This match ended in a draw. Better luck to both players next time."

let rec GameLoop turn matrix =
    Console.Clear ()
    let player = 2 - (turn % 2)
    printfn "Player %i's turn" player
    PrintMatrix matrix
    let index = GetIndex matrix
    let newMatrix = ChangeIndex player index matrix
    if HasWon player newMatrix then PrintWin player newMatrix
    elif not (List.contains 0 newMatrix) then PrintDraw ()
    else GameLoop (turn+1) newMatrix

[<EntryPoint>]
let main _ =
    let matrix = [for _ in 0..8 do yield 0]
    let turn = 1
    printfn "Welcome!\nThis is a game of noughts and crosses.\nPlease decide between yourselves who is player 1 and player 2.\nPress [Enter] to continue."
    Console.ReadLine () |> ignore
    GameLoop turn matrix
    Console.ReadLine () |> ignore
    0