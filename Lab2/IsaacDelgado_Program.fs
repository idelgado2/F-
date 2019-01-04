(*************************************************************************
 * Survey Of Programming Lanugages - (F#) Lab2
 * Isaac Delgado
 * COSC_3353
 * 2/19/2017
 * Purpose: This program will define multiple functions based on the questions
 *			provided by the Lab2 questions
 *
 * Input:   If these functinos are tested in F# interactive make sure the
 *			the exceptions are passed to F# interactive before hand.
 *
 **************************************************************************)

exception Empty_List_Generated
exception Negative_Input
exception Empty_String_Was_Passed


///Exercise 2.1 (part 1)
///This function return a list contaning integers
///the list contains consecutive integers in a decending pattern
///starting from the integer passed to this function 
///(e,g downTo 5 ---> int list [5;4;3;2;1])
let downTo n = if n = 0 then raise Empty_List_Generated
               elif n < 0 then raise Negative_Input
               elif n = 1 then [1]
               else [n .. -1 .. 1]

///Exercise 2.1 (part 2)
///This function return a list contaning integers
///the list contains consecutive integers in a decending pattern
///starting from the integer passed to this function 
///(e.g downTo 5 ---> int list [5;4;3;2;1])
///**However implemented with mattern matching**
let downTo2 n = match n with
                |0 -> raise Empty_List_Generated
                |j when j < 0 -> raise Negative_Input
                |1 -> [1]
                |_ -> [n .. -1 .. 1]


///Exercise 2.2
///This function returns a new list contaning integers
///a list will be passed to this function and this function
///will remove all odd indexed values from the list
///(e.g mylist = [5;4;3;2;1]
///        removeOddIdx mylist ---> int list = [5;3;1])
let removeOddIdx someList = match someList with
                            |[] -> []
                            |[x] -> [x]
                            |head::tail -> [for i in 0 .. 2 .. ((List.length <| someList) - 1) -> someList.Item(i)]


///Exercise 2.3
///This function will take an integer list as a parameter
///and return a new list of tuples containg a pairings
///of the items in the original intger list with it their
///adjacent item
///***If there are odd number of items the last item will
///    be removed/ignored***
///(e.g mylist = [5;4;3;2;1]
///        combinePair mylist ---> (int * int) list = [(5, 4); (3, 2)])
let combinePair (someList : int list) = match someList with
                                        |[] -> []
                                        |[x] -> []
                                        |head::tail -> 
                                                if (List.length <| someList) % 2 = 0 then
                                                          let even = List.filter (fun x -> x % 2 = 0) someList
                                                          let odd = List.filter (fun x -> x % 2 <> 0) someList
                                                          List.zip odd even
                                                else
                                                    let newList = List.truncate ((List.length <| someList) - 1) someList
                                                    let even = List.filter (fun x -> x % 2 = 0) newList
                                                    let odd = List.filter (fun x -> x % 2 <> 0) newList
                                                    List.zip odd even


///Exercise 2.4 (part 1)
///This function will take a string that is passed
//and create a char list seperating each letter in the string
///(e.g myWord = "hello"
///        explode myWord ---> char list = ['h';'e';'l';'l';'o';])
let explode (word : string) = match word with
                              |"" -> raise Empty_String_Was_Passed
                              |_ -> (word.ToCharArray()) |> List.ofArray

///Exercise 2.4 (part 2)
///This function will take a string that is passed
//and create a char list seperating each letter in the string
///(e.g myWord = "hello"
///        explode myWord ---> char list = ['h';'e';'l';'l';'o';])
///*** This implemented through recurstion ***
let rec explode2 (word : string) = match word.Length with
                                                   |0 -> raise Empty_String_Was_Passed
                                                   |1 -> [word.Chars(0)]
                                                   |_ -> word.Chars(0) :: (explode2 <| word.Remove(0,1))

///Exercise 2.5 (part 1)
///This functino will take a char list and 
///combine all the characters (items in the list)
///into an individual string
///(e.g myCharList = ['h'; 'e'; 'l'; 'l'; 'o']
///        implode myCharList ---> string = "hello")
let implode (charList : char list) = List.foldBack (fun character myString -> character.ToString() + myString) charList ""

///Exercise 2.5 (part 2)
///This functino will take a char list and 
///combine all the characters (items in the list)
///into an individual string in *Reversed order*
///(e.g myCharList = ['h'; 'e'; 'l'; 'l'; 'o']
///        implode myCharList ---> string = "olleh")
let implodeRev (charList : char list) = List.fold (fun myString character -> character.ToString() + myString) "" charList

///Exercise 2.6 (part-1, & part-2 & part-3)
///This function will take a string and convert all letters
///to upper case if not already
///(e.g myWord = "hello"
///      toUpper myWord ---> "HELLO")
let toUpper (word : string) = implode <| ((List.map (fun x -> System.Char.ToUpper(x))) <| (explode word))
let toUpper1 = explode >> (List.map (fun x -> System.Char.ToUpper(x))) >> implode
let toUpper2 (word : string) = (((List.map (fun x -> System.Char.ToUpper(x))) << explode) word) |> implode


///Exercise 2.7
///This function return a boolean (true or false)
///if the given string is a palindrome or not (case Insensative)
///(e.g myWord = "racecar"
///      toUpper myWord ---> "HELLO")
let palindrome (word : string) = let explodedWord = explode (toUpper word) 
                                 let reversedWord = List.rev <| explodedWord
                                 explodedWord.Equals(reversedWord)

///Exercise 2.8
///This function implements the resursive Ackermann function
///This function will take a tuple of two integers
///(e.g ack(3, 11) ----> 16381
let rec ack (m : int, n : int) = match (m,n) with
                                 |(m, n) when m = 0 && n <> 0 -> (n + 1) 
                                 |(m, n) when m > 0 && n = 0 -> ack(m-1, 1)
                                 |(m, n) when m > 0 && n > 0 -> ack(m-1, ack(m, n-1))
                                 |(_,_) -> raise Negative_Input

//ack (3, 11) ---- val it : int = 16381

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
