(**********************************************************************************
 * Survey Of Programming Lanugages - (F#) Lab3
 * Isaac Delgado
 * COSC_3353
 * 3/6/2017
 * Purpose: This program will implement the Neutron game. This game is
 *          played by two players who each have 5 pieces and one neutron
 *          piece in the center. The goal of the game is to get the netron
 *          piece to your respective side.
 *
 * Input:   The player must enter what piece they wnat to move and then
 *          choose where on the board they want to move this piece. The
 *          board has letters on top and numbers on the side thus indicating
 *          where you want to move your piece is determeined by the comination
 *          of letter and number. (e.g A1, B4, D2, etc.)
 *
 * Progress: This program is not finished. At the moment you are only able to
 *           move one piece anywhere you like on the according to the game rules
 *           You cannot move the neutron at this point, which is actually the 
 *           the remaining aspect of this game. As this assignment is already late
 *           and I must work on other assingments I will stop at this point
 *
 ************************************************************************************)

module Array2d
exception SoldierNotFound
exception InvalidSoliderColor
exception SamePosition
exception NoneEmptyPosition
exception CannotMove
exception Empty_String_Was_Passed
exception InvalidLetter
exception InvalidNumber

open System

let boardLetters = [|"A"; "B"; "C"; "D"; "E"|]


//these are all the player pieces
let mutable B_1 = (1, 0) 
let mutable B_2 = (1, 1) 
let mutable B_3 = (1, 2) 
let mutable B_4 = (1, 3) 
let mutable B_5 = (1, 4)
let mutable R_1 = (5, 0) 
let mutable R_2 = (5, 1) 
let mutable R_3 = (5, 2) 
let mutable R_4 = (5, 3) 
let mutable R_5 = (5, 4)

let mutable NEUTRON = (3, 2)

let mutable TotalMoves = ref 0

//enumerated types for deciding whos turn it is in the game
type soldierColor = 
    |black = 0
    |red = 1

///This function will simply print out the entire board
///by its indexes and not show the actual object on each spot
let gameBoardIndexes = Array2D.init 6 6 (fun x y -> (x,y))

///This funnction will create the actual board with the game pieces and
///the neutron in the center. additionally Ive added a row on top
///to indicate the coulumn by letter and the very far right column
///to indicate each row by number
let gameBoard = Array2D.init 6 6 (fun x y -> if (x = 1 && y <> 5) then  "Black_" + Convert.ToString(y+1) 
                                              elif (x = 5 && y <> 5) then  "Red___" + Convert.ToString(y+1)
                                              elif (x = 3 && y = 2) then "Neutron"
                                              elif (x = 0 && y <> 5) then "-- " + boardLetters.[y] + " --"
                                              elif (y = 5 && x <> 0) then "-- " + Convert.ToString(x) + " --"
                                              else "       ")

///This function will print out the board game currently
///this will be used to show the changes made to the board
let printOut (gameBoard : string [,]) = printfn "%A" gameBoard

///This function is insane and I can believe I got it to work
///This function finds the validity of a piece moving to the
///the players desired spot. This is accordance to the neutron rules
///however the pieces are only designed to go vertically or horizontally
///diagonally is not developed into this function at this time
let moveSoldier (currentPosition : int * int) (desiredPosition : int * int) (gameBoard : string [,])  = 
                                          if (((fst currentPosition) = (fst desiredPosition)) && ((snd currentPosition) = (snd desiredPosition)))
                                             then false
                                          elif (gameBoard.[(fst desiredPosition), (snd desiredPosition)] <> "       ")
                                             then false
                                          elif (((fst currentPosition) = (fst desiredPosition)) && ((snd currentPosition) <> (snd desiredPosition)))
                                             then if((snd currentPosition) < (snd desiredPosition)) then
                                                       let mutable temp = ((snd currentPosition) + 1)
                                                       let mutable continueLoop = true 
                                                      
                                                       while((temp <> snd desiredPosition) && continueLoop) do
                                                           if(gameBoard.[(fst currentPosition), temp] <> "       ") then
                                                               continueLoop <- false
                                                           else
                                                               temp <- temp + 1
                                                       continueLoop
                                                  else
                                                      let mutable temp = ((snd currentPosition) - 1)
                                                      let mutable continueLoop = true

                                                      while ((temp <> snd desiredPosition) && continueLoop) do
                                                          if(gameBoard.[(fst currentPosition), temp] <> "       ") then
                                                              continueLoop <- false
                                                          else
                                                              temp <- temp - 1
                                                      continueLoop
                                          elif(((fst currentPosition) <> (fst desiredPosition)) && ((snd currentPosition) = (snd desiredPosition))) 
                                             then if((fst currentPosition) < (fst desiredPosition)) then
                                                       let mutable temp = ((fst currentPosition) + 1)
                                                       let mutable continueLoop = true 
                                                      
                                                       while((temp <> fst desiredPosition) && continueLoop) do
                                                           if(gameBoard.[temp, (snd currentPosition)] <> "       ") then
                                                               continueLoop <- false
                                                           else
                                                               temp <- temp + 1
                                                       continueLoop
                                                  else
                                                      let mutable temp = ((fst currentPosition) - 1)
                                                      let mutable continueLoop = true

                                                      while ((temp <> fst desiredPosition) && continueLoop) do
                                                          if(gameBoard.[temp, (snd currentPosition)] <> "       ") then
                                                              continueLoop <- false
                                                          else
                                                              temp <- temp - 1
                                                      continueLoop
                                                     
                                          else raise CannotMove

///This function will sheck if the desired move is valid
///If so this functino will proceed to change the values in the 
///boardgame 2d array, by replacing the previous spot with a 
///blank and the desired spot with the piece that the player is movin
let playerTurn soldier destination (gameBoard : string [,]) = if(moveSoldier soldier destination gameBoard) then
                                                                 gameBoard.[fst destination, snd destination] <- gameBoard.[fst soldier, snd soldier]
                                                                 gameBoard.[fst soldier, snd soldier] <- "       "
                                                                 true
                                                              else
                                                                 false 
///Exercise 2.4 (part 1)
///This function will take a string that is passed
//and create a char list seperating each letter in the string
///(e.g myWord = "hello"
///        explode myWord ---> char list = ['h';'e';'l';'l';'o';])
let explode (word : string) = match word with
                              |"" -> raise Empty_String_Was_Passed
                              |_ -> (word.ToCharArray()) |> List.ofArray


///This function will take the coloumn choice and return the 
///the corresponding coloumn index for computing
let letterChoice x = match x with
                     |'a' -> 0
                     |'b' -> 1
                     |'c' -> 2
                     |'d' -> 3
                     |'e' -> 4
                     |_ -> raise InvalidLetter


///This function will take the row choice and return the 
///the corresponding row index for computing
let rowChoice x = match x with
                  |"1" -> 1
                  |"2" -> 2
                  |"3" -> 3
                  |"4" -> 4
                  |"5" -> 5
                  |_ -> raise InvalidNumber


///This function will take the corresponding color of whos turn it is
///the soldier's number of what piece the player chooses
///and returns the corresponding index of the piece
let SoldierChoice soldier soldierNumber (gameBoard : string [,]) = match soldier with
                                                                   |soldierColor.black -> match soldierNumber with 
                                                                                          |"1" -> (1,0)
                                                                                          |"2" -> (1,1)
                                                                                          |"3" -> (1,2)
                                                                                          |"4" -> (1,3)
                                                                                          |"5" -> (1,4)
                                                                                          |_ -> raise InvalidNumber
                                                                   |soldierColor.red -> match soldierNumber with 
                                                                                          |"1" -> (5,0)
                                                                                          |"2" -> (5,1)
                                                                                          |"3" -> (5,2)
                                                                                          |"4" -> (5,3)
                                                                                          |"5" -> (5,4)
                                                                                          |_ -> raise InvalidNumber
                                                                   |_-> raise SoldierNotFound


[<EntryPoint>]
let main argv = 
    let mutable myChoice = ""           //to temporarily hold what soldier the player wants to move
    let mutable column = ""             //to hold what coloumn the player wants to move the piece
    let mutable row = ""                //to hold what row the player wants to move the piece
    let mutable loop = true             //validity for if the porgram should keep running
    let mutable whosTurn = 1            //1 is red's turn, 0 is black's turn
    let mutable soldierChoice = (0,0)   //to hold index of the solider chosen by the player
    let board = gameBoard
    let mutable quit = ""               //To see if player wants to quit the game
    printOut board

    printfn "Lets play Neutron!!!"

    while(loop) do 

     if(whosTurn = 1) then   //if red players turn 
         printf "player_1 (RED),  what piece would you like to Move?:"
         myChoice <- System.Console.ReadLine()
         soldierChoice <- SoldierChoice soldierColor.red myChoice board

     else                   //else its black players turn
         printf "player_1 (BLACK),  what piece would you like to Move?:"
         myChoice <- System.Console.ReadLine()
         soldierChoice <- SoldierChoice soldierColor.black myChoice board


     printfn "\nWhere would you like to move this piece?: "
     printfn "Column Choices: A, B, C, D, E"
     printf "Column (Please type in the letter):"

     column <- System.Console.ReadLine()

     let columnInt = letterChoice (Char.ToLower((Convert.ToChar(column))))

     printfn "Row Choices: 1, 2, 3, 4, 5"
     printf "Row (Please type in the Row Number):"

     row <- System.Console.ReadLine()

     let rowInt = rowChoice row

     printfn "Moving %A to position (%A, %A)" gameBoard.[fst soldierChoice, snd soldierChoice] rowInt columnInt 

     let turn = playerTurn soldierChoice (rowInt, columnInt) gameBoard
     if(turn = true) then printOut gameBoard   //if piece was succesfully moved then print out the board to see change

     if(turn = false) then        //if piece was NOT successfully moved then print out error
      printfn "**Invalid move**"
      printOut gameBoard

     printf "Would you like to quit the game? (Y/N)"    
     quit <- System.Console.ReadLine()      //I dont know how wuit this game elequently so I will just ask you every turn
     if( (quit = "Y") || (quit = "y")) then loop <-false

     if(whosTurn = 1) then whosTurn <- 0 else whosTurn <- 1   //switch player turns every iteration of the loop

    0 // return an integer exit code
