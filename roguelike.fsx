
// open System

type Color = System.ConsoleColor
type Direction = Left | Right | Down | Up

let worldSizeX = 75
let worldSizeY = 75
let screenSizeX = 50
let screenSizeY = 10


let randomNumber (lower: int) (upper: int) =
    let random = new System.Random()
    random.Next(lower, upper)




// MARK: Canvas

type Canvas () =

    let mutable _screen = Array2D.create screenSizeY screenSizeX ("  ", Color.White, Color.DarkBlue)

    member this.SetScreen screen = 
        _screen <- screen

    member this.Get (x:int, y:int) =
        _screen.[y,x]

    member this.Set (x: int, y: int, c: string, fg: Color, bg: Color) =
        _screen.[y,x] <- (c, bg, fg)

    member this.Show () =

        System.Console.CursorVisible <- false
        System.Console.SetCursorPosition(0,0)

        
        for y = 0 to Array2D.length1 _screen - 1 do
            for x = 0 to Array2D.length2 _screen - 1 do
                let c, fg, bg = _screen.[y,x]
                
                
                System.Console.ForegroundColor <- fg

                System.Console.BackgroundColor <- bg
                    
                System.Console.Write(c)
            System.Console.ResetColor()
            System.Console.Write("\n")
        System.Console.ResetColor()
        
        // let mutable fromX = 0
        // let mutable toX = 0
        // let mutable fromY = 0
        // let mutable toY = 0
        // let radX = 10
        // let radY = 40


        // if posX >= 0 && posY >= 0 then
        //     if posX - radX >= 0 then
        //         fromX <- posX - radX
        //     else fromX <- 0

        //     if posX + radX <= Array2D.length1 screen - 1 then
        //         toX <- posX + radX
        //     else
        //         toX <- Array2D.length1 screen - 1

        //     if posY - radY >= 0 then
        //         fromY <- posY - radY
        //     else fromY <- 0

        //     if posY + radY <= Array2D.length2 screen - 1 then
        //         toY <- posY + radY
        //     else
        //         toY <- Array2D.length2 screen - 1
                
        //     for x = fromX to toX do
        //         for y = fromY to toY do
        //             let c, fg, bg = screen.[x,y]
        //             System.Console.ForegroundColor <- fg
        //             System.Console.BackgroundColor <- bg
        //             System.Console.Write(c)
        //         System.Console.Write("\n")
        //     System.Console.ResetColor()
        // else ()










[<AbstractClass>]
type Entity () =
    abstract member Update: unit -> unit
    default this.Update () = ()

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


[<AbstractClass>]
type Object () =
    inherit Entity ()

    abstract member InteractWith: Creature -> unit

    abstract member FullyOccupy: bool

    abstract member Color: Color










// MARK: World

type World (canvas: Canvas, x:int, y:int, map: (Entity option * Object) [,]) =
    let mutable _world = map
    let mutable _objects = []

    let mutable _player: Creature option = None

    member this.world = _world

    member this.AddObject (object: Object, x: int, y: int) =
        _world.[y,x] <- (None, object)

    member this.AddPlayer (player: Creature) = _player <- Some player

    member this.MoveEntity (fromPos: (int * int), toPos: (int * int), entity: Entity) =
        let backgroundFrom = snd _world.[snd fromPos, fst fromPos]
        let backgroundTo = snd _world.[snd toPos, fst toPos]
        _world.[snd fromPos, fst fromPos] <- (None, backgroundFrom)
        _world.[snd toPos, fst toPos] <- (Some entity, backgroundTo)

    member this.worldCutOut =
        
        let playerX, playerY = 
            if _player.IsSome then _player.Value.Position
            else (10,10)

        let fromX = 
            if playerX - screenSizeX / 2 < 0 then
                0
            else playerX - screenSizeX / 2
        
        let fromY =
            if playerY - screenSizeY / 2 < 0 then
                0
            else playerY - screenSizeY / 2
        
        let toX =
            if playerX + screenSizeX / 2 > worldSizeX - 1 then
                worldSizeX - 1
            else playerX + screenSizeX / 2
        
        let toY =
            if playerY + screenSizeY / 2 > worldSizeY - 1 then
                worldSizeY - 1
            else playerY + screenSizeY / 2

        printfn "Player %A %A From %A %A To %A %A" playerX playerY fromX fromY toX toY

        let GetBlock (item: Entity option, background: Object) =
            let char =
                match item with
                | Some x -> x.Icon
                | None -> "  "
            (char, Color.White, background.Color)
        
        (Array2D.map GetBlock _world).[fromY..toY, fromX..toX]





    member this.Play () =

        canvas.SetScreen this.worldCutOut
        canvas.Show ()

        let mutable gameEnded = false
        while not gameEnded do

            while System.Console.KeyAvailable = false do
                System.Threading.Thread.Sleep(250)
            
            if _player.IsSome then _player.Value.Update ()
            canvas.SetScreen this.worldCutOut
            canvas.Show ()









// MARK: Player

type Player (x:int, y:int, world: World) =
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

    member this.MoveTo (x: int, y: int) =
        let curX, curY = this.Position
        // let c, bg, fg = canvas.Get (curX, curY)
        // canvas.Set (curX, curY, "  ", fg, bg)
        world.MoveEntity (this.Position, (x,y), this)
        this.Position <- (x,y)

        // this.RenderOn (canvas)

    override this.Icon = "😇"
    
    member this.HandleKeypress () =
        let x, y = this.Position
        let key = System.Console.ReadKey()
        match key.Key with
        | System.ConsoleKey.UpArrow when y > 0 -> this.MoveTo (x, y - 1)
        | System.ConsoleKey.DownArrow when y < worldSizeY - 1 -> this.MoveTo (x, y + 1)
        | System.ConsoleKey.LeftArrow when x > 0 -> this.MoveTo (x - 1, y)
        | System.ConsoleKey.RightArrow when x < worldSizeX - 1 -> this.MoveTo (x + 1, y)
        | _ -> ()

    override this.Update () =
        this.HandleKeypress ()











// MARK: Enemy
type Enemy (x:int, y:int) =
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

    member this.MoveIn (direction: Direction) =
        let (x, y) = _position

        match direction with
        | Direction.Up -> _position <- (x, y-1)
        | Direction.Down -> _position <- (x, y+1)
        | Direction.Left -> _position <- (x-1, y)
        | Direction.Right -> _position <- (x+1, y)


    member this.MoveTowards (player: Player) =
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










// MARK: World objects

type Grass () =
    inherit Object ()

    override this.InteractWith (creature: Creature) = ()

    override this.FullyOccupy = false

    override this.Color = Color.Green

    

type Wall (startPosition: (int*int)) =
    inherit Object ()

    override this.InteractWith (creature: Creature) = ()

    override this.FullyOccupy = true

    member this.position = startPosition
    
    override this.Color = Color.Black


type Water () =
    inherit Object ()

    override this.InteractWith (creature: Creature) = creature.Heal 2

    override this.FullyOccupy = false

    override this.Color = Color.Blue


type Fire () =
    inherit Object ()

    let mutable interactions = 0
    let mutable isBurning = true

    override this.InteractWith (creature: Creature) =
        if isBurning then creature.Damage 1

        if interactions >= 5 then isBurning <- false

    override this.FullyOccupy = false

    override this.Color = Color.Red


type FleshEatingPlant () =
    inherit Object ()

    override this.InteractWith (creature: Creature) = creature.Damage 5

    override this.FullyOccupy = true

    override this.Color = Color.DarkGreen


type Exit () =
    inherit Object ()

    override this.InteractWith (creature: Creature) = 
        // Show end game notice
        System.Console.Clear ()
        printfn "You won!!!!"

    override this.FullyOccupy = false

    override this.Color = Color.White






            


let test = Canvas ()



System.Console.Clear ()


let map = Array2D.create worldSizeX worldSizeY (None, Grass () :> Object)

let world = World (test, worldSizeX, worldSizeY, map)

let player = Player (10, 10, world)
world.AddPlayer player

let wall = Wall ((2,2))
let wall2 = Wall ((5,5))
let wall3 = Wall ((10,10))
let wall4 = Wall ((7,7))

world.AddObject(wall, 2, 2)
world.AddObject(wall2, 5, 5)
world.AddObject(wall3, 10, 10)
world.AddObject(wall4, 7, 7)


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



    
