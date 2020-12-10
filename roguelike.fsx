
// open System

type Color = System.ConsoleColor

type Direction = Left | Right | Down | Up

let randomNumber (lower: int) (upper: int) =
    let random = new System.Random()
    random.Next(lower, upper)


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

// test.Show ()

// test.Set (3, 4, ' ', Color.White, Color.Blue)
// test.Set (2, 2, ' ', Color.White, Color.Blue)
// test.Set (3, 4, ' ', Color.White, Color.Red)

// // Tegner en lang streg når man shower for anden gang. Hvilket ikke er meningen
// test.Show ()

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

    member this.Position = position

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

