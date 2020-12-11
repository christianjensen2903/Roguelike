
// open System

type Color = System.ConsoleColor

type Canvas (rows: int, cols: int) =

    let mutable screen = Array2D.create rows cols (" ", Color.White, Color.DarkBlue)

    member this.Rows = rows
    member this.Cols = cols

    member this.Set (x: int, y: int, c: string, fg: Color, bg: Color) =
        screen.[y,x] <- (c, bg, fg)

    member this.ResetScreen () =
        screen <- Array2D.create rows cols (" ", Color.White, Color.DarkBlue)

    member this.Show () =
        System.Console.Clear ()

        for y = 0 to Array2D.length1 screen - 1 do
            for x = 0 to Array2D.length2 screen - 1 do
                let c, fg, bg = screen.[y,x]
                System.Console.ForegroundColor <- fg
                System.Console.BackgroundColor <- bg
                System.Console.Write(c)
                System.Console.ResetColor()
            System.Console.Write("\n")



// test.Set (2, 2, ' ', Color.White, Color.Blue)
// test.Set (3, 4, ' ', Color.White, Color.Red)

// // Tegner en lang streg når man shower for anden gang. Hvilket ikke er meningen

[<AbstractClass>]
type Entity () =
    abstract member RenderOn: Canvas -> unit
    default this.RenderOn (canvas: Canvas) = ()


type Player () =
    inherit Entity ()

    let mutable position = (0,0)
    let mutable hitPoints = 10
    let mutable isDead = false

    member this.HitPoints = hitPoints

    member this.IsDead = isDead

    member this.Damage (dmg: int) =
        hitPoints <- hitPoints - dmg
    
    member this.Heal (h: int) =
        hitPoints <- hitPoints + h

    member this.MoveTo (x: int, y: int) =
        position <- (x,y)


[<AbstractClass>]
type Item () =
    inherit Entity ()

    abstract member InteractWith: Player -> unit

    abstract member FullyOccupy: bool
    

type Wall () =
    inherit Item ()

    override this.InteractWith (player: Player) = ()

    override this.FullyOccupy = true


type Water () =
    inherit Item ()

    override this.InteractWith (player: Player) = player.Heal 2

    override this.FullyOccupy = false


type Fire () =
    inherit Item ()

    let mutable interactions = 0
    let mutable isBurning = true

    override this.InteractWith (player: Player) =
        if isBurning then player.Damage 1

        if interactions >= 5 then isBurning <- false

    override this.FullyOccupy = false


type FleshEatingPlant () =
    inherit Item ()

    override this.InteractWith (player: Player) = player.Damage 5

    override this.FullyOccupy = true


type Exit () =
    inherit Item ()

    override this.InteractWith (player: Player) = 
        // Show end game notice
        System.Console.Clear ()
        printfn "You won!!!!"

    override this.FullyOccupy = false








// MARK: Start Menu

type StartMenu (canvas: Canvas) =

    let mutable _selection = 0


    member this.DrawMenu (options: string list) =
       
        let padding = 1
        let optionsLength = List.length options
        for i in 0 .. optionsLength - 1 do
            let option = options.[i]
            let mutable x = canvas.Cols / 2 - (String.length option) / 2
            let y = canvas.Rows / 2 - optionsLength / 2 * (padding + 1) + i * (padding + 1)
            
            for char in Seq.toList option do

                if _selection = i then
                    // If selection is the same as option make text another color
                    canvas.Set (x, y, string char, Color.DarkBlue, Color.Black) 
                else
                    canvas.Set (x, y, string char, Color.DarkBlue, Color.White)

                x <- x + 1
        
        canvas.Show ()

    member this.ControlMenu (options: string list) =
        let mutable showMenu = true
        while showMenu do
            let key = System.Console.ReadKey()

            match key.Key with
            | System.ConsoleKey.UpArrow -> 
                if _selection > 0 then 
                    _selection <- _selection - 1
            | System.ConsoleKey.DownArrow -> 
                if _selection < List.length options - 1 then 
                    _selection <- _selection + 1
            | System.ConsoleKey.Enter ->
                showMenu <- false
            | _ -> ()

            this.DrawMenu options
        
    member this.MenuScreen () =

        let menuOptions = ["New Game"; "Continue Game"]

        canvas.Show ()
        this.DrawMenu menuOptions
        
        this.ControlMenu menuOptions

        
        match _selection with
            | 0 ->
                this.ClassScreen ()
            | 1 ->
                // Continue game
                ()
            | _ -> ()
    
    member this.ClassScreen () =

        let classesOptions = ["Warrior"; "Hunter"; "Mage"]

        canvas.ResetScreen ()
        this.DrawMenu classesOptions

        this.ControlMenu classesOptions
        
        // TODO: convert classes to fsharp classes
        match _selection with
            | 0 ->
                // Select warrior
                ()
            | 1 ->
                // Select hunter
                ()
            | 2 ->
                // Select mage
                ()
            | _ -> ()


let canvas = Canvas (20,40)

let menu = StartMenu canvas

menu.MenuScreen ()



    