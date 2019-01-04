(*************************************************************************
 * Survey Of Programming Lanugages - (F#) - Lab 1
 * Isaac Delgado
 * COSC_3353
 * 2/4/2017
 * Purpose: This program will define three different functions (add10, add20
 *			, and add_mn) that will add 10, 20, or a multiple of 10 to a passed
 *			integer.
 *
 * Input:   Each functino will be passed a single function expect for add_mn
 *			which will be passed the integer to be added to and a second integer
 *			that will determine how many 10's to be added to the first integer
 *
 * Output:  The output for add10, add20 and add_mn will be the passed integer
 *			+ 10, the passed integer + 20, and the passed integer + (the
 *			the second passed integer x 10) respectively
 **************************************************************************)
#light

module Lab1C

///This function will add 10 to the passed integer (e.g add10 10 = 15)
let add10 x = x + 10

///This function will add 20 to the passed integer (e.g add10 10 = 20)
let add20 x = add10 (add10 x)

///This function will add n * 10 to m (e.g add_mn 3 5 = 53 (3 + 10 + 10 + 10 + 10 + 10))
let rec add_mn m n = match (m, n) with 
                     |(m,0) ->  m
                     |(m,n) -> add_mn (add10 m) (n - 1) 
