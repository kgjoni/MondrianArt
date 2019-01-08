//
// F# program to generate random Mondrian Art.
//
// KRISTI GJONI
// U. of Illinois, Chicago
// CS 341, Spring 2018
// Project #05
//

#light

//
// randomInt LB UB
//
// generates random integer in range LB..UB, inclusive.
//
// NOTE: if you want repeatable random numbers for testing,
// uncomment "let seed = 0".  If you want random images 
// every time, uncomment the other "let seed = ..." line.
//
let seed = System.DateTime.Now.Millisecond
//let seed = 0
let ranInt = new System.Random(seed)
let randomInt LB UB =
  if LB <= UB then
    ranInt.Next(LB, UB+1)
  else
    LB


//
// randomRect
//
// An example of generating a random-colored rectangle
// in HTML SVG format.
//
let randomRect x1 y1 x2 y2 = 
  let red = randomInt 0 255
  let green = randomInt 0 255
  let blue = randomInt 0 255
  let white = 255
  let html = 
       "<line x1=" + (string x1) +
       " y1 =" + (string y1) + 
       " x2 =" + (string (x2 - x1 + 1)) + 
       " y2 =" + (string (y2 - y1 + 1)) + 
       " stroke =\"black\" stroke-width=\"3" +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
  html


// Helper function to split a rectangle and add border lines
let split x1 y1 x2 y2 = 
  let red = randomInt 0 255
  let green = randomInt 0 255
  let blue = randomInt 0 255
  let html =
       "<line x1=" + (string x1) +
       " y1 =" + (string y1) + 
       " x2 =" + (string (x2 - x1 + 1)) + 
       " y2 =" + (string (y2 - y1 + 1)) +
       " stroke =\"black\" stroke-width=\"3" +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
  html
  

// Generate a random color
let genColor number =
  if number < 9 then 
    "255, 0, 0" // red
  else if number < 17 then
    "135, 206, 250" // blue
  else if number < 25 then
    "255, 255, 0" // yellow
  else 
    "255, 255, 255" // white


//
// _mondrian x1 y1 x2 y2 canvasWidth canvasHeight
//
// Recursive helper function that randomly generates an image
// for the area denoted by the rectange (x1,y1) and (x2,y2),
// where (x1,y1) is the upper-left corner and (x2,y2) is the 
// lower-right corner.  The image is in HTML SVG format.
//
let rec _mondrian x1 y1 x2 y2 canvasWidth canvasHeight = 
  
  // Declare random height to be in range 0.33 && 0.67
  let randomHeight_y1 = 253     // canvasHeight * 0.33 = 253
  let randomHeight_y2 = 514     // canvasHeight * 0.67 = 514

  // Declare random width to be in range 0.33 && 0.67
  let randomWidth_x1 = 338      // canvasWidth * 0.33 = 338
  let randomWidth_x2 = 686      // canvasWidth * 0.67 = 686
  
  // region is wider than half the initial canvas width and region is taller than half the initial canvas height
  if (((x2 - x1 + 1) > (canvasWidth/2)) && ((y2 - y1 + 1) > (canvasHeight/2))) then

    // Random width and height
    let randomHeight = randomInt randomHeight_y1 randomHeight_y2
    let randomWidth = randomInt randomWidth_x1 randomWidth_x2

    // Horizontal split
    let horizontalSplit = 
      "<line x1=0" + 
      " y1=" + (string randomHeight) +
      " x2=" + (string canvasWidth) +
      " y2=" + (string randomHeight) + 
      " stroke=\"black\"" +
      " stroke-width=\"3\" />\n"

    // Verticals split
    let verticalSplit = 
      "<line x1=" + (string randomWidth) + 
      " y1=0"  +
      " x2=" + (string randomWidth) +
      " y2=" + (string canvasHeight) + 
      " stroke=\"black\"" +
      " stroke-width=\"3\" />\n"

    // Add the rectangles
    let first_rect = _mondrian x1 y1 randomWidth randomHeight canvasWidth canvasHeight
    let second_rect = _mondrian randomWidth randomHeight canvasWidth canvasHeight canvasWidth canvasHeight
    second_rect + first_rect + horizontalSplit + verticalSplit 

  // region is wider than half the initial canvas width
  else if ((x2 - x1 + 1) > canvasWidth/2) then

    // Random width and height
    let randomHeight1 = randomInt randomHeight_y1 randomHeight_y2
    let randomWidth1 = randomInt randomWidth_x1 randomWidth_x2
    
    // Vertical split
    let verticalSplit = 
      "<line x1=" + (string randomWidth1) + 
      " y1=0"  +
      " x2=" + (string randomWidth1) +
      " y2=" + (string canvasHeight) + 
      " stroke=\"black\"" +
      " stroke-width=\"3\" />\n"
    
    // Add the rectangles
    let first_rect = _mondrian x1 y1 randomWidth1 randomHeight1 canvasWidth canvasHeight
    let second_rect = _mondrian randomWidth1 randomHeight1 canvasWidth canvasHeight canvasWidth canvasHeight
    first_rect + second_rect + verticalSplit

  // region is taller than half the initial canvas height
  else if ((y2 - y1 + 1) > canvasHeight/2) then

    // Random width and height
    let randomHeight2 = randomInt randomHeight_y1 randomHeight_y2
    let randomWidth2 = randomInt randomWidth_x1 randomWidth_x2

    // Horizontal split
    let horizontalSplit = 
      "<line x1=0" + 
      " y1=" + (string randomHeight2) +
      " x2=" + (string canvasWidth) +
      " y2=" + (string randomHeight2) + 
      " stroke=\"black\"" +
      " stroke-width=\"3\" />\n"

    // Add the rectangles
    let first_rect = _mondrian x1 y1 randomWidth2 randomHeight2 canvasWidth canvasHeight
    let second_rect = _mondrian randomWidth2 randomHeight2 canvasWidth canvasHeight canvasWidth canvasHeight
    first_rect + second_rect + horizontalSplit 

  // region is big enough to split both horizontally and vertically, and both a horizontal and vertical split are randomly selected
  else

    // Random width and height
    let randomWidth3 = randomInt 120 (x2 + (x2 / 2))
    let randomHeight3 = randomInt 120 (y2 + (y2 / 2))

    if ((randomWidth3 < (x2 - x1 + 1)) && (randomHeight3 < (y2 - y1 + 1))) then

      // Vertical split
      let verticalSplit = 
        "<line x1=" + (string randomWidth3) + 
        " y1=0"  +
        " x2=" + (string randomWidth3) +
        " y2=" + (string canvasHeight) + 
        " stroke=\"black\"" +
        " stroke-width=\"3\" />\n"

      // Horizontal split
      let horizontalSplit = 
        "<line x1=0" + 
        " y1=" + (string randomHeight3) +
        " x2=" + (string canvasWidth) +
        " y2=" + (string randomHeight3) + 
        " stroke=\"black\"" +
        " stroke-width=\"3\" />\n"

      // Add the rectangles
      let first_rect = _mondrian x1 y1 randomWidth3 randomHeight3 canvasWidth canvasHeight
      first_rect + verticalSplit + horizontalSplit

    // region is tall enough to split vertically, a vertical split is randomly selected
    else if (randomWidth3 < (x2 - x1 + 1)) then

      // Vertical split
      let verticalSplit = 
        "<line x1=" + (string randomWidth3) + 
        " y1=0"  +
        " x2=" + (string randomWidth3) +
        " y2=" + (string canvasHeight) + 
        " stroke=\"black\"" +
        " stroke-width=\"3\" />\n"

      // Add the rectangles
      let first_rect = _mondrian x1 y1 randomWidth3 randomHeight3 canvasWidth canvasHeight
      first_rect + verticalSplit

    // region is wide enough to split horizontally, and a horizontal split is randomly selected
    else if (randomHeight3 < (y2 - y1 + 1)) then

      // Horizontal split
      let horizontalSplit = 
        "<line x1=0" + 
        " y1=" + (string randomHeight3) +
        " x2=" + (string canvasWidth) +
        " y2=" + (string randomHeight3) + 
        " stroke=\"black\"" +
        " stroke-width=\"3\" />\n"

      // Add the rectangles
      let first_rect = _mondrian x1 y1 randomWidth3 randomHeight3 canvasWidth canvasHeight
      first_rect + horizontalSplit
 
    // Fill the current region (randomly, either white or colored, and if colored, with arandom determination of red, blue or yellow)
    else 

      // Get a random number 0-100
      let r = randomInt 0 100

      if (r < 9) then
      // RED

        let html = 
          "<rect x=" + (string (x1)) +
          " y=" + (string (y1)) + 
          " width=" + (string (x2 - x1 + 1)) + 
          " height=" + (string (y2 - y1 + 1)) + 
          " stroke=\"black\"" +
          " stroke-width=\"4\"" +
          " fill=rgb(255, 0, 0) />\n"
        html

      else if (r < 17) then
      // SKYBLUE

        let html = 
          "<rect x=" + (string (x1)) +
          " y=" + (string (y1)) + 
          " width=" + (string (x2 - x1 + 1)) + 
          " height=" + (string (y2 - y1 + 1)) + 
          " stroke=\"black\"" +
          " stroke-width=\"4\"" +
          " fill=rgb(135, 206, 250) />\n"
        html

      else if (r < 25) then
      // YELLOW

        let html = 
          "<rect x=" + (string (x1)) +
          " y=" + (string (y1)) + 
          " width=" + (string (x2 - x1 + 1)) + 
          " height=" + (string (y2 - y1 + 1)) + 
          " stroke=\"black\"" +
          " stroke-width=\"4\"" +
          " fill=rgb(255, 255, 0) />\n"
        html

      else
      // WHITE

        let html = 
          "<rect x=" + (string (x1)) +
          " y=" + (string (y1)) + 
          " width=" + (string (x2 - x1 + 1)) + 
          " height=" + (string (y2 - y1 + 1)) + 
          " stroke=\"black\"" +
          " stroke-width=\"4\"" +
          " fill=rgb(255, 255, 255) />\n"
        html


//
// mondrian canvasWidth canvasHeight
//
// Randomly generates an image in the spirit of Piet Mondrian.
// Returns an HTML document containing an SVG image of the given
// canvas width and height.  
//
// SVG: https://www.w3schools.com/html/html5_svg.asp
//
let mondrian canvasWidth canvasHeight = 
  let prefix = "<html>\n<head></head>\n<body>\n" +
               "<svg width=\"" + (string canvasWidth) + 
               "\" height=\"" + (string canvasHeight) + "\">\n"
  //
  let image = _mondrian 0 0 (canvasWidth-1) (canvasHeight-1) canvasWidth canvasHeight
  // 

  let suffix = "</svg>\n</body>\n</html>\n"
  let html = prefix + image + suffix
  html


//
// main:
//
[<EntryPoint>]
let main argv =
  printfn "** Starting **"
  //
  let width = 1024
  let height = 768
  let filename = "..\\..\\..\\mondrian.html"  // same folder as F# code:
  //
  printfn "** Generating image... "
  let html = mondrian width height
  System.IO.File.WriteAllText(filename, html) 
  //
  printfn "** Done **"
  0
