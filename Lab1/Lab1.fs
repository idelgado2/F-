(*************************************************************************
 * Survey Of Programming Lanugages - (F#) - Lab 1
 * Isaac Delgado
 * COSC_3353
 * 2/4/2017
 * Purpose: This program is a suplement to to Lab1. This porgram will be uploaded
 *			into f# interacitve and used to demonstrate, interacitve loadability
 *			and different scope association
 *
 * Input:   N/A
 *
 * Output:  When loaded into F# interacitve the defined varibales are viewable
 *			and available as resource.
 **************************************************************************)
#light

module Lab1
let x = 42
let myName = "Aragorn"
let age = 85
let y = 4 + 2
let a = 5
let b = let a = 10 in a + 5
let c = a + b
