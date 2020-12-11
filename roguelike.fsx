
// open System

type Color = System.ConsoleColor

let worldSizeX = 75
let worldSizeY = 75

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
