open Roguelike

System.Console.Clear ()
let canvas = Canvas (screenSizeX,screenSizeY)

let menu = StartMenu canvas

// Shows the class Screen as the first thing when the program starts
menu.ClassScreen ()