
// open System

type Color = System.ConsoleColor

type Canvas (rows: int, cols: int) =

    let mutable screen = Array2D.create rows cols (' ', Color.White, Color.Red)

    member this.Set (x: int, y: int, c: char, fg: Color, bg: Color) =
        screen.[x,y] <- (c, bg, fg)

    member this.Show () =
        System.Console.Clear ()
        
        for x = 0 to Array2D.length1 screen - 1 do
            for y = 0 to Array2D.length2 screen - 1 do
                let c, fg, bg = screen.[x,y]
                // printfn "%A %A %A" c fg bg
                System.Console.ResetColor()
                System.Console.ForegroundColor <- fg
                System.Console.BackgroundColor <- bg
                System.Console.SetCursorPosition(x,y)
                System.Console.Write(c)

            // System.Console.WriteLine()


let test = Canvas (10,10)

test.Show ()

test.Set (3, 4, ' ', Color.White, Color.Blue)
test.Set (2, 2, ' ', Color.White, Color.Blue)
test.Set (3, 4, ' ', Color.White, Color.Red)

// Tegner en lang streg når man shower for anden gang. Hvilket ikke er meningen
test.Show ()