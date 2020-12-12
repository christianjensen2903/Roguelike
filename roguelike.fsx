
// open System

type Color = System.ConsoleColor
type Direction = Left | Right | Down | Up

let worldSizeX = 100
let worldSizeY = 200
let screenSizeX = 50
let screenSizeY = 30


let randomNumber (lower: int) (upper: int) =
    let random = new System.Random()
    random.Next(lower, upper)


// MARK: Canvas

type Canvas (rows: int, cols: int) =

    let mutable _screen = Array2D.create cols rows ("  ", Color.Green, Color.Green)


    member this.Get (x:int, y:int) =
        _screen.[y,x]

    member this.Set (x: int, y: int, c: string, fg: Color, bg: Color) =
        _screen.[y,x] <- (c, bg, fg)

    member this.ShowMenu () =

        System.Console.CursorVisible <- false
        System.Console.SetCursorPosition(0,0)

        for y = 0 to Array2D.length1 _screen - 1 do
            for x = 0 to Array2D.length2 _screen - 1 do
                let c, fg, bg = _screen.[y,x]
                
                
                System.Console.ForegroundColor <- fg

                System.Console.BackgroundColor <- bg
                    
                System.Console.Write(c)
            System.Console.Write("\n")
        System.Console.ResetColor()
    
    member this.Show (playerX, playerY) =
        System.Console.CursorVisible <- false
        System.Console.SetCursorPosition(0,0)

        let fromX, toX = 
            if playerX - (screenSizeX / 2) < 0 then
                0, screenSizeX - 1
            else if playerX + (screenSizeX / 2) > worldSizeX - 1 then
                worldSizeX - 1 - screenSizeX, worldSizeX - 1
            else playerX - (screenSizeX / 2), playerX + (screenSizeX / 2)
        
        let fromY, toY = 
            if playerY - (screenSizeY / 2) < 0 then
                0, screenSizeY - 1
            else if playerY + (screenSizeY / 2) > worldSizeY - 1 then
                worldSizeY - 1 - screenSizeY, worldSizeY - 1
            else playerY - (screenSizeY / 2), playerY + (screenSizeY / 2)
        
        let cutout = _screen.[fromY .. toY, fromX .. toX]

        for y = 0 to screenSizeY - 1 do
            for x = 0 to screenSizeX - 1 do
                let c, fg, bg = cutout.[y,x]
                System.Console.ForegroundColor <- fg
                System.Console.BackgroundColor <- bg
                System.Console.Write(c)
            System.Console.Write("\n")
        System.Console.ResetColor()







[<AbstractClass>]
type Entity () =
    abstract member RenderOn: Canvas -> unit
    default this.RenderOn (canvas: Canvas) = ()


    abstract member Icon: string
    default this.Icon = "  "


[<AbstractClass>]
type Creature () =
    inherit Entity ()

    abstract member HitPoints: int with get, set

    abstract member Position: (int*int) with get, set

    abstract member IsDead: bool with get, set

    abstract member Damage: int -> unit
    
    abstract member Heal: int -> unit

    abstract member Update: (Entity option * Item) [,] -> unit
    default this.Update (world: (Entity option * Item) [,]) = ()

and [<AbstractClass>] Item () =
    inherit Entity ()

    abstract member InteractWith: Creature -> unit

    abstract member FullyOccupy: bool

    abstract member Color: Color

    abstract member Update: (Entity option * Item) [,] -> unit
    default this.Update (world: (Entity option * Item) [,]) = ()









// MARK: Player

type Player (x:int, y:int, canvas: Canvas) =
    inherit Creature ()

    let mutable _position = (x,y)
    let mutable _hitPoints = 10
    let mutable _isDead = false

    override this.HitPoints
        with get () = _hitPoints
        and set (value) = _hitPoints <- value

    override this.Position
        with get () = _position
        and set (value) = _position <- value

    override this.IsDead
        with get () = _isDead
        and set (value) = _isDead <- value

    override this.Damage (dmg: int) =
        _hitPoints <- _hitPoints - dmg
    
    override this.Heal (h: int) =
        _hitPoints <- _hitPoints + h

    override this.Icon = "😇"

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         let _, fg, bg = canvas.Get (x,y)
         canvas.Set(x, y, this.Icon, fg, bg)

    member this.MoveTo (x: int, y: int, world: (Entity option * Item) [,]) =
        let oldX,oldY = this.Position
        let _, fg, bg = canvas.Get (oldX,oldY)
        let field = world.[y,x]
        let item = snd field
        printfn "test: %A %A" (fst field).IsSome (item.FullyOccupy = false)
        if not (fst field).IsSome || item.FullyOccupy = false then
            canvas.Set(oldX, oldY, "  ", fg, bg)
            world.[oldY, oldX] <- (None, snd world.[oldY, oldX])
            world.[y,x] <- (Some (this :> Entity), item)
            this.Position <- (x,y)
            item.InteractWith this
        else
            ()
    

    member this.HandleKeypress world =
        let mutable x, y = this.Position
        let key = System.Console.ReadKey()

        match key.Key with
        | System.ConsoleKey.UpArrow when y > 0 -> y <- y - 1
        | System.ConsoleKey.DownArrow when y < worldSizeY - 1 -> y <- y + 1
        | System.ConsoleKey.LeftArrow when x > 0 -> x <- x - 1
        | System.ConsoleKey.RightArrow when x < worldSizeX - 1 -> x <- x + 1
        | _ -> ()

        this.MoveTo (x, y, world)

    override this.Update world =
        this.HandleKeypress world
        this.RenderOn (canvas)










// MARK: Enemy
type Enemy (x:int, y:int, canvas: Canvas, player: Player, world: (Entity option * Item) [,]) =
    inherit Creature ()

    let world = world
    let canvas = canvas
    let player = player
    let mutable _position = (x,y)
    let mutable _hitPoints = 10
    let mutable _isDead = false

    override this.HitPoints
        with get () = _hitPoints
        and set (value) = _hitPoints <- value

    override this.Position
        with get () = _position
        and set (value) = _position <- value

    override this.IsDead
        with get () = _isDead
        and set (value) = _isDead <- value

    override this.Damage (dmg: int) =
        _hitPoints <- _hitPoints - dmg
    
    override this.Heal (h: int) =
        _hitPoints <- _hitPoints + h
    
    override this.Icon = "🧟‍♀️"

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         let _, fg, bg = canvas.Get (x,y)
         canvas.Set(x, y, this.Icon, fg, bg)

    member this.MoveIn (direction: Direction) =
        
        let oldX,oldY = _position
        printfn "1 %A %A" oldX oldY
        let mutable newX, newY = oldX, oldY
        let _, fg, bg = canvas.Get (oldX,oldY)
        printfn "2 %A %A" oldX oldY

        match direction with
        | Direction.Up when oldY > 0 -> newY <- oldY - 1
        | Direction.Down when oldY < worldSizeY - 2 -> newY <- oldY + 1
        | Direction.Left when oldX > 0 -> newX <- oldX - 1
        | Direction.Right when oldX < worldSizeX - 2 -> newX <- oldX + 1
        

        let field = world.[newY,newX]
        printfn "3 %A %A" newX newY
        let item = snd field
        
        if not (fst field).IsSome || item.FullyOccupy = false then
            printfn "4 %A %A" newX newY
            canvas.Set(oldX, oldY, "  ", fg, bg)
            printfn "5 %A %A" newX newY
            world.[oldY, oldX] <- (None, snd world.[oldY, oldX])
            world.[y,x] <- (Some (this :> Entity), item)
            _position <- (newX,newY)
            item.InteractWith this
        else
            printfn "6"
            ()


    member this.MoveTowardsPlayer () =
        // Get positions
        let (enemyX, enemyY) = _position
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
                if (dx < 0) then
                    dir <- Direction.Right
                else
                    dir <- Direction.Left
            
            else
                if (dy < 0) then
                    dir <- Direction.Down
                else
                    dir <- Direction.Up
            
            this.MoveIn dir

    override this.Update world =
        this.MoveTowardsPlayer ()
        this.RenderOn canvas







// MARK: World objects

type Grass () =
    inherit Item ()

    override this.InteractWith (creature: Creature) = ()

    override this.FullyOccupy = false

    override this.Color = Color.Green

    

type Wall (startPosition: (int*int)) =
    inherit Item ()

    override this.InteractWith (creature: Creature) = ()

    override this.FullyOccupy = true

    member this.position = startPosition

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.position
         canvas.Set(x, y, "  ", Color.Black, Color.Black)
    
    override this.Color = Color.Black


type Water () =
    inherit Item ()

    override this.InteractWith (creature: Creature) = creature.Heal 2

    override this.FullyOccupy = false

    override this.Color = Color.Blue


type Fire (startPosition: (int*int)) =
    inherit Item ()

    let mutable interactions = 0
    let mutable isBurning = true

    member this.position = startPosition

    override this.InteractWith (creature: Creature) =
        if isBurning then creature.Damage 1

        if interactions >= 5 then isBurning <- false

    override this.FullyOccupy = false

    override this.Color = Color.Red

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.position
         canvas.Set(x, y, "  ", Color.Red, Color.Red)


type FleshEatingPlant () =
    inherit Item ()

    override this.InteractWith (creature: Creature) = creature.Damage 5

    override this.FullyOccupy = true

    override this.Color = Color.DarkGreen


type Exit () =
    inherit Item ()

    override this.InteractWith (creature: Creature) = 
        // Show end game notice
        System.Console.Clear ()
        printfn "You won!!!!"

    override this.FullyOccupy = false

    override this.Color = Color.White







// MARK: World

type World (canvas: Canvas, x:int, y:int) =
    let mutable _world: (Entity option * Item) [,] = Array2D.create x y (None, (Grass () :> Item))

    member this.world = _world

    member this.AddItem (item: Item, x:int, y:int) =
         _world.[y,x] <- (fst _world.[y,x], item)
         item.RenderOn canvas


    member this.Play () =

        let player = Player (20,50,canvas)
        let enemy = Enemy (3, 20, canvas, player, this.world)

        player.RenderOn canvas
        enemy.RenderOn canvas
        canvas.Show (fst player.Position, snd player.Position)

        let mutable gameEnded = false
        while not gameEnded do

            while System.Console.KeyAvailable = false do
                // enemy.Update (_world)
                canvas.Show (fst player.Position, snd player.Position)
                System.Threading.Thread.Sleep(250)

            player.Update (_world)
            
            
 



            


let test = Canvas (200,200)

System.Console.Clear ()

let world = World (test, worldSizeX, worldSizeY)

let wall = Wall ((2,2))
let wall2 = Wall ((5,5))
let wall3 = Wall ((10,10))
let wall4 = Fire ((7,7))

world.AddItem(wall, 2, 2)
world.AddItem(wall2, 5, 5)
world.AddItem(wall3, 10, 10)
world.AddItem(wall4, 7, 7)


world.Play ()










// MARK: Start Menu

// type StartMenu (canvas: Canvas) =

//     let mutable _selection = 0


//     member this.DrawMenu (options: string list) =
       
//         let padding = 1
//         let optionsLength = List.length options
//         for i in 0 .. optionsLength - 1 do
//             let option = options.[i]
//             let mutable x = worldSizeX / 2 - (String.length option) / 2
//             let y = worldSizeY / 2 - optionsLength / 2 * (padding + 1) + i * (padding + 1)
            
//             for char in Seq.toList option do

//                 if _selection = i then
//                     // If selection is the same as option make text another color
//                     canvas.Set (x, y, string char, Color.DarkBlue, Color.Black) 
//                 else
//                     canvas.Set (x, y, string char, Color.DarkBlue, Color.White)

//                 x <- x + 1
        
//         canvas.Show ()

//     member this.ControlMenu (options: string list) =
//         let mutable showMenu = true
//         while showMenu do
//             let key = System.Console.ReadKey()

//             match key.Key with
//             | System.ConsoleKey.UpArrow -> 
//                 if _selection > 0 then 
//                     _selection <- _selection - 1
//             | System.ConsoleKey.DownArrow -> 
//                 if _selection < List.length options - 1 then 
//                     _selection <- _selection + 1
//             | System.ConsoleKey.Enter ->
//                 showMenu <- false
//             | _ -> ()

//             this.DrawMenu options
        
//     member this.MenuScreen () =

//         let menuOptions = ["New Game"; "Continue Game"]

//         canvas.Show ()
//         this.DrawMenu menuOptions
        
//         this.ControlMenu menuOptions

        
//         match _selection with
//             | 0 ->
//                 this.ClassScreen ()
//             | 1 ->
//                 // Continue game
//                 ()
//             | _ -> ()
    
//     member this.ClassScreen () =

//         let classesOptions = ["Warrior"; "Hunter"; "Mage"]

//         canvas.ResetScreen ()
//         this.DrawMenu classesOptions

//         this.ControlMenu classesOptions
        
//         // TODO: convert classes to fsharp classes
//         match _selection with
//             | 0 ->
//                 // Select warrior
//                 ()
//             | 1 ->
//                 // Select hunter
//                 ()
//             | 2 ->
//                 // Select mage
//                 ()
//             | _ -> ()


// let canvas = Canvas (20,40)

// let menu = StartMenu canvas

// menu.MenuScreen ()



    
