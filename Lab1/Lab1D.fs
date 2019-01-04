(*************************************************************************
 * Survey Of Programming Lanugages - (F#) - Lab 1
 * Isaac Delgado
 * COSC_3353
 * 2/4/2017
 * Purpose: This program will define two different functions max2 & max_list.
 *			max2 function will return the greates integer between two. The 
 *			Function max_list will calculate the greatest element in an integer list 
 *
 * Input:   max2 will be passed two integers as input while max_list will be
 *			passed a list as input
 *
 * Output:  The OutPut for both max2 and max_list will be a single integer
 **************************************************************************)

#light

module Lab1D

///exception to indicate the two integer values are equal
exception Args_are_Equal

///exception to indicate that a list is empty
exception Empty_List

///This function will take in 2 integers as parameters
///These integers will be compared and the greatest one will be returned
///If they are equal then an exception will be raised
let max2 x y = match compare x y with
               |t when t > 0 -> x
               |t when t < 0 -> y
               |_ -> raise Args_are_Equal

///This function will be passed an integer list and will return the greates value in the list.
///If the list is empty then an exception will be raised
///***This function was reference from http://mariusbancila.ro/blog/tag/f-tutorial-article-list-recursive/ ***
let rec max_list (someList:int list) = match someList with 
                                       |[] -> raise Empty_List
                                       |[x] -> x
                                       |x::l -> max2 x (max_list l)
