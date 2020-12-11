
// open System

type Color = System.ConsoleColor

let worldSizeX = 75
let worldSizeY = 75
type Direction = Left | Right | Down | Up

let randomNumber (lower: int) (upper: int) =
    let random = new System.Random()
    random.Next(lower, upper)


type Canvas (rows: int, cols: int) =

    let mutable screen = Array2D.create rows cols ("  ", Color.Green, Color.Green)

    member this.Get (x:int, y:int) =
        screen.[x,y]

    member this.Set (x: int, y: int, c: string, fg: Color, bg: Color) =
        screen.[x,y] <- (c, bg, fg)

    member this.Show (posX, posY) =
        System.Console.CursorVisible <- false
        System.Console.SetCursorPosition(0,0)
        let mutable fromX = 0
        let mutable toX = 0
        let mutable fromY = 0
        let mutable toY = 0
        let radX = 10
        let radY = 40

        if posX >= 0 && posY >= 0 then
            if posX - radX >= 0 then
                fromX <- posX - radX
            else fromX <- 0

            if posX + radX <= Array2D.length1 screen - 1 then
                toX <- posX + radX
            else
                toX <- Array2D.length1 screen - 1

            if posY - radY >= 0 then
                fromY <- posY - radY
            else fromY <- 0

            if posY + radY <= Array2D.length2 screen - 1 then
                toY <- posY + radY
            else
                toY <- Array2D.length2 screen - 1
                
            for x = fromX to toX do
                for y = fromY to toY do
                    let c, fg, bg = screen.[x,y]
                    System.Console.ForegroundColor <- fg
                    System.Console.BackgroundColor <- bg
                    System.Console.Write(c)
                System.Console.Write("\n")
            System.Console.ResetColor()
        else ()
        
[<AbstractClass>]
type Entity () =
    abstract member RenderOn: Canvas -> unit
    default this.RenderOn (canvas: Canvas) = ()


type Player (x:int, y:int, canvas: Canvas) =
    inherit Entity ()

    let mutable position = (x,y)
    let mutable hitPoints = 10
    let mutable isDead = false

    member this.currentPosition = position

    member this.HitPoints = hitPoints

    member this.Position = position

    member this.IsDead = isDead

    member this.Damage (dmg: int) =
        hitPoints <- hitPoints - dmg
    
    member this.Heal (h: int) =
        hitPoints <- hitPoints + h

    member this.MoveTo (x: int, y: int) =
        let curX, curY = this.currentPosition
        let c, bg, fg = canvas.Get (curX, curY)
        canvas.Set (curX, curY, "  ", fg, bg)
        position <- (x,y)
        this.RenderOn (canvas)

    default this.RenderOn (canvas: Canvas) =
        let x,y = this.currentPosition
        canvas.Set(x, y, "🥰", Color.Green, Color.Black)
        canvas.Show (x,y)
    
    member this.HandleKeypress () =
        let x, y = this.currentPosition
        let key = System.Console.ReadKey()
        match key.Key with
        | System.ConsoleKey.UpArrow when x > 0 -> this.MoveTo (x - 1, y)
        | System.ConsoleKey.DownArrow when x < worldSizeX - 1 -> this.MoveTo (x + 1, y)
        | System.ConsoleKey.LeftArrow when y > 0 -> this.MoveTo (x, y - 1)
        | System.ConsoleKey.RightArrow when y < worldSizeY - 1 -> this.MoveTo (x, y + 1)
        | _ -> ()



[<AbstractClass>]
type Item () =
    inherit Entity ()

    abstract member InteractWith: Player -> unit

    abstract member FullyOccupy: bool

type Grass () =
    inherit Item ()

    override this.InteractWith (player: Player) = ()

    override this.FullyOccupy = false

    default this.RenderOn (canvas: Canvas) = ()
    

type Wall (startPosition: (int*int)) =
    inherit Item ()

    override this.InteractWith (player: Player) = ()

    override this.FullyOccupy = true

    member this.position = startPosition
    default this.RenderOn (canvas: Canvas) =
        let x,y = this.position
        canvas.Set(x, y, "  ", Color.Black, Color.Black)
        canvas.Show (-1,-1)


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

type World (canvas: Canvas, x:int, y:int) =
    let mutable _world = Array2D.create x y (Grass () :> Entity)
    let mutable _objects = []
    member this.world = _world

    member this.AddItem (item:Item, x:int, y:int) =
        _world.[x,y] <- item :> Entity
        item.RenderOn canvas

    member this.Play () =
        let player = Player (10,50,canvas)
        player.RenderOn canvas

        let mutable gameEnded = false
        while not gameEnded do
            
            while System.Console.KeyAvailable = false do
                System.Threading.Thread.Sleep(250)
            
            player.HandleKeypress ()
            


let test = Canvas (worldSizeX,worldSizeY)



System.Console.Clear ()

let world = World (test, worldSizeX, worldSizeY)

let wall = Wall ((2,2))
let wall2 = Wall ((5,5))
let wall3 = Wall ((10,10))
let wall4 = Wall ((7,7))

world.AddItem(wall, 2, 2)
world.AddItem(wall2, 5, 5)
world.AddItem(wall3, 10, 10)
world.AddItem(wall4, 7, 7)


world.Play ()


type Enemy () =
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

    member this.MoveIn (direction: Direction) =
        let (x, y) = position

        match direction with
        | Direction.Up -> position <- (x, y-1)
        | Direction.Down -> position <- (x, y+1)
        | Direction.Left -> position <- (x-1, y)
        | Direction.Right -> position <- (x+1, y)


    member this.MoveTowards (player: Player) =
        // Get positions
        let (enemyX, enemyY) = position
        let (playerX, playerY) = player.Position

        // Calculate distance to player
        let dx = enemyX - playerX
        let dy = enemyY - playerY
        let dis = int (sqrt (float(dx)**2. + float(dy)**2.))

        let directions = [Direction.Left; Direction.Right; Direction.Up; Direction.Down]

        if (dis > 10) then  
            // If player are to far away move random direction
            let dirInd = randomNumber 0 (List.length directions)
            let dir = directions.[dirInd]
            this.MoveIn dir
        
        else
            let mutable dir = Direction.Left

            // Move in the direction with biggest difference in position
            if (abs dx > abs dy) then
                if (dx > 0) then
                    dir <- Direction.Right
                else
                    dir <- Direction.Left
            
            else
                if (dy > 0) then
                    dir <- Direction.Down
                else
                    dir <- Direction.Up
            
            this.MoveIn dir








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



    
