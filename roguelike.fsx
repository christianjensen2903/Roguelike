
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

    let mutable _screen = Array2D.create cols rows ("  ", Color.DarkGreen, Color.DarkGreen)


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

    abstract member Update: unit -> unit
    default this.Update () = ()

    abstract member Icon: string
    default this.Icon = "  "

    


[<AbstractClass>]
type Creature () =
    inherit Entity ()

    abstract member Position: (int * int) with get, set  

    abstract member HitPoints: int with get, set

    abstract member IsDead: bool with get, set

    abstract member Damage: int -> unit
    
    abstract member Heal: int -> unit


and [<AbstractClass>] Item () =
    inherit Entity ()

    abstract member Position: (int * int)
    default this.Position = (0,0)

    abstract member InteractWith: Creature -> unit

    abstract member FullyOccupy: bool








[<AbstractClass>]
type Spell () =
    abstract member baseDmg: int


type MultiShot () =
    inherit Spell ()
    override this.baseDmg = 5

type FreezingShot () =
    inherit Spell ()
    override this.baseDmg = 5

type Poisonshot () =
    inherit Spell ()
    override this.baseDmg = 5

type Fireblast () =
    inherit Spell ()
    override this.baseDmg = 5

type Freezenova () =
    inherit Spell ()
    override this.baseDmg = 5

type Lightningbolt () =
    inherit Spell ()
    override this.baseDmg = 5







[<AbstractClass>]
type InvItem () =
    inherit Item ()

    abstract member name: string

    abstract member icon: string
    
    abstract member stats: Map<string, int>

    override this.InteractWith (creature: Creature) =
        // Pickup item -> add to inv 
        ()

    override this.FullyOccupy = false


type Weapon (name: string, icon: string, dmg: int, spellpower: int, speed: int) =
    inherit InvItem ()

    let mutable _position = (0,0)
    override this.Position = _position

    override this.name = name

    override this.icon = icon

    override this.stats =
       [ "Dmg.", dmg;
          "Spell power", spellpower;
          "Speed", speed]
        |> Map.ofList


let basicBow = Weapon ("Basic Bow", "🏹", 2, 1, 2)
let basicSword = Weapon ("Basic Sword", "🗡", 2, 1, 1)
let basicStaff = Weapon ("Basic Staff", "🪄", 1, 3, 1)





type Stat = Damage | Health | Spellpower | Armor | Speed | MagicResistance | Critchance | Critdamage

[<AbstractClass>]
type RpgClass () =
    abstract member startingWeapon: Weapon

    abstract member spells: Spell list

    abstract member statMultipliers: Map<Stat, int>


type Hunter () =
    inherit RpgClass ()

    override this.startingWeapon = basicBow

    override this.spells =
        [MultiShot ();
        FreezingShot ();
        Poisonshot ()]

    override this.statMultipliers =
        [Stat.Damage, 2;
        Stat.Health, 2;
        Stat.Spellpower, 1;
        Stat.Armor, 2;
        Stat.Speed, 2;
        Stat.MagicResistance, 2;
        Stat.Critchance, 2;
        Stat.Critdamage, 2]
        |> Map.ofList

type Warrior () =
    inherit RpgClass ()

    override this.startingWeapon = basicSword

    override this.spells =
        [MultiShot ();
        FreezingShot ();
        Poisonshot ()]

    override this.statMultipliers =
        [Stat.Damage, 3;
        Stat.Health, 4;
        Stat.Spellpower, 1;
        Stat.Armor, 3;
        Stat.Speed, 1;
        Stat.MagicResistance, 3;
        Stat.Critchance, 2;
        Stat.Critdamage, 2]
        |> Map.ofList

type Mage () =
    inherit RpgClass ()

    override this.startingWeapon = basicStaff

    override this.spells =
        [Fireblast ();
        Freezenova ();
        Lightningbolt ()]

    override this.statMultipliers =
        [Stat.Damage, 1;
        Stat.Health, 1;
        Stat.Spellpower, 4;
        Stat.Armor, 1;
        Stat.Speed, 2;
        Stat.MagicResistance, 3;
        Stat.Critchance, 1;
        Stat.Critdamage, 1]
        |> Map.ofList



type Projectile (startPosition: (int * int), icon: string, dmg: int, canvas: Canvas, world: (Entity option * Item) [,], direction: Direction) =
    inherit Item ()

    let mutable _removed = false
    let mutable _position = startPosition
    override this.Position = _position

    override this.InteractWith (creature: Creature) = creature.Damage dmg

    override this.FullyOccupy = false

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         let _, fg, bg = canvas.Get (x,y)
         canvas.Set(x, y, icon, fg, bg)

    member this.Remove () =
        printfn "åhhhh"
        let x, y = _position
        let _, fg, bg = canvas.Get (x,y)
        canvas.Set(x, y, "  ", fg, bg)
        world.[y, x] <- (None, snd world.[y, x])
        _removed <- true

           

    override this.Update () =
        if not _removed then
            
            let oldX,oldY = _position
            let mutable newX, newY = oldX, oldY
            let _, fg, bg = canvas.Get (oldX,oldY)

            match direction with
            | Up when oldY > 0 -> newY <- oldY - 1
            | Down when oldY < worldSizeY - 2 -> newY <- oldY + 1
            | Left when oldX > 0 -> newX <- oldX - 1
            | Right when oldX < worldSizeX - 2 -> newX <- oldX + 1
            | _ -> this.Remove ()

            let field = world.[newY,newX]
            let item = snd field
            
            if not (fst field).IsSome && item.FullyOccupy = false then
                canvas.Set(oldX, oldY, "  ", fg, bg)
                world.[oldY, oldX] <- (None, snd world.[oldY, oldX])
                world.[newY,newX] <- (Some (this :> Entity), item)
                _position <- (newX,newY)
                if not _removed then this.RenderOn canvas
                
            else if (fst field).IsSome then
                match (fst field).Value with
                | :? Creature -> this.InteractWith ((fst field).Value :?> Creature)
                | _ -> ()

                this.Remove ()

            else
                this.Remove ()
        else ()
            














// MARK: Player

type Player (x:int, y:int, rpgClass: RpgClass, canvas: Canvas, world: (Entity option * Item) [,]) =
    inherit Creature ()

    let mutable _position = (x,y)
    let mutable _hitPoints = 10
    let mutable _isDead = false
    let mutable _target: Enemy option = None
    let mutable _attackTimer = 0

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


    member this.SwitchTarget () =
        let withinDistance (elm: (Entity option * Item)) =
            let object = fst elm
            if _target.IsSome then _target.Value.RemoveTarget ()

            if object.IsSome then
                match object.Value with
                | :? Enemy ->
                    // Get positions
                    let enemy = object.Value :?> Enemy
                    let (enemyX, enemyY) = enemy.Position
                    let (playerX, playerY) = _position

                    // Calculate distance to player
                    let dx = enemyX - playerX
                    let dy = enemyY - playerY
                    let dis = int (sqrt (float(dx)**2. + float(dy)**2.))
                    if dis < 20 then Some enemy else None

                | _ -> None
            else None

        let mutable nearbyEnemies: Enemy list = []
        for x in [0..(Array2D.length1 world) - 1] do 
                for y in [0..(Array2D.length2 world) - 1] do
                    let enemy = withinDistance(world.[x, y])
                    if enemy.IsSome then nearbyEnemies <- nearbyEnemies @ [enemy.Value]

        if List.length nearbyEnemies - 1 >= 0 then
            let target = nearbyEnemies.[randomNumber 0 (List.length nearbyEnemies - 1)]
            _target <- Some target
            target.Target ()
        else ()
    
    member this.Attack () =

        // Get positions
        let (enemyX, enemyY) = _target.Value.Position
        let (playerX, playerY) = _position

        // Calculate distance to player
        let dx = enemyX - playerX
        let dy = enemyY - playerY
        let dis = int (sqrt (float(dx)**2. + float(dy)**2.))

        match rpgClass with
        | :? Hunter ->
            printfn "ATTACK"

        | :? Warrior -> if dis < 2 then _target.Value.Damage 3

        | :? Mage -> 
            printfn "ATTACK"
            // Shoot in the direction with biggest difference in position
            let mutable dir = Direction.Left
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

            (Projectile ((playerX, playerY), "🟠", 2, canvas, world, dir)).Update ()
            // let mutable projX, projY = _position
            // match dir with
            // | Up when projY > 0 -> projY <- projY - 1
            // | Down when projY < worldSizeY - 2 -> projY <- projY + 1
            // | Left when projX > 0 -> projX <- projX - 1
            // | Right when projX < worldSizeX - 2 -> projX <- projX + 1
            // | _ -> ()

            // if (fst world.[projY,projX])
        
        | _ -> ()
        _attackTimer <- 5

    override this.Icon = "😇"

    member this.UpdateAttackCounter () =
        if _attackTimer > 0 then _attackTimer <- _attackTimer - 1

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         let _, fg, bg = canvas.Get (x,y)
         canvas.Set(x, y, this.Icon, fg, bg)

    member this.MoveTo (x: int, y: int) =
        let oldX,oldY = this.Position
        let _, fg, bg = canvas.Get (oldX,oldY)
        let field = world.[y,x]
        let item = snd field
        if not (fst field).IsSome && item.FullyOccupy = false then
            canvas.Set(oldX, oldY, "  ", fg, bg)
            world.[oldY, oldX] <- (None, snd world.[oldY, oldX])
            world.[y,x] <- (Some (this :> Entity), item)
            this.Position <- (x,y)
            item.InteractWith this
        else
            ()
    

    member this.HandleKeypress () =
        let mutable x, y = this.Position
        let key = System.Console.ReadKey()
        
        match key.Key with
        | System.ConsoleKey.UpArrow when y > 0 -> y <- y - 1
        | System.ConsoleKey.DownArrow when y < worldSizeY - 1 -> y <- y + 1
        | System.ConsoleKey.LeftArrow when x > 0 -> x <- x - 1
        | System.ConsoleKey.RightArrow when x < worldSizeX - 1 -> x <- x + 1
        | System.ConsoleKey.Spacebar when _target.IsSome -> if _attackTimer = 0 then this.Attack ()
        | System.ConsoleKey.Tab -> this.SwitchTarget ()
        | _ -> ()

        this.MoveTo (x, y)

    override this.Update () =
        this.HandleKeypress ()
        this.RenderOn (canvas)










// MARK: Enemy
and Enemy (x:int, y:int, canvas: Canvas, player: Player, world: (Entity option * Item) [,]) =
    inherit Creature ()

    let world = world
    let canvas = canvas
    let player = player
    let mutable _position = (x,y)
    let mutable _hitPoints = 10
    let mutable _isDead = false
    let mutable _isTarget = false
    let mutable _spawnTimer = 0


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

        if _hitPoints <= 0 then
            this.Die ()
    
    member this.Die () =
        printfn "Dead"
        let x, y = _position
        let item = snd world.[y,x]
        item.RenderOn canvas
        world.[y,x] <- (None, item)
        _spawnTimer <- 100
        _isDead <- true


    override this.Heal (h: int) =
        _hitPoints <- _hitPoints + h

    member this.Target () = _isTarget <- true
    member this.RemoveTarget () = _isTarget <- false

    member this.Attack () =
        // Get positions
        let (enemyX, enemyY) = _position
        let (playerX, playerY) = player.Position

        // Calculate distance to player
        let dx = enemyX - playerX
        let dy = enemyY - playerY
        let dis = int (sqrt (float(dx)**2. + float(dy)**2.))

        if dis <= 1 then
            printfn "Haps"
            player.Damage 5
    
    override this.Icon = "🧟‍♀️"

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         let _, fg, bg = canvas.Get (x,y)
         world.[y,x] <- (Some (this :> Entity), snd world.[y,x])

         if _isTarget then
            canvas.Set(x, y, this.Icon, Color.Red, bg)
         else
            canvas.Set(x, y, this.Icon, fg, bg)

    member this.MoveIn (direction: Direction) =
        
        let oldX,oldY = _position
        let mutable newX, newY = oldX, oldY
        let _, fg, bg = canvas.Get (oldX,oldY)

        match direction with
        | Direction.Up when oldY > 0 -> newY <- oldY - 1
        | Direction.Down when oldY < worldSizeY - 2 -> newY <- oldY + 1
        | Direction.Left when oldX > 0 -> newX <- oldX - 1
        | Direction.Right when oldX < worldSizeX - 2 -> newX <- oldX + 1
        | _ -> ()
        

        let field = world.[newY,newX]
        let item = snd field
        
        if not (fst field).IsSome && item.FullyOccupy = false then
            (snd world.[oldY, oldX]).RenderOn canvas
            // canvas.Set(oldX, oldY, "  ", fg, bg)
            world.[oldY, oldX] <- (None, snd world.[oldY, oldX])
            
            _position <- (newX,newY)
            item.InteractWith this
        else
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
            if (abs dx < abs dy) then
                if (dx < 0) then
                    dir <- Direction.Right
                else
                    dir <- Direction.Left
            
            else
                if (dy > 0) then
                    dir <- Direction.Down
                else
                    dir <- Direction.Up
            
            this.MoveIn dir

    override this.Update () =
        if not _isDead then
            this.MoveTowardsPlayer ()
            this.Attack ()
            this.RenderOn canvas







// MARK: World objects

type Grass (startPosition) =
    inherit Item ()

    override this.Position = startPosition

    override this.InteractWith (creature: Creature) = ()

    override this.FullyOccupy = false

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "  ", Color.DarkGreen, Color.DarkGreen)





type Wall (startPosition) =
    inherit Item ()

    override this.Position = startPosition

    override this.InteractWith (creature: Creature) = ()

    override this.FullyOccupy = true

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "  ", Color.Black, Color.Black)
    



type Water (startPosition) =
    inherit Item ()

    override this.Position = startPosition

    override this.InteractWith (creature: Creature) = creature.Heal 2

    override this.FullyOccupy = false

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "  ", Color.Green, Color.Green)



type Fire (startPosition) =
    inherit Item ()

    let mutable interactions = 0
    let mutable isBurning = true

    override this.Position = startPosition

    override this.InteractWith (creature: Creature) =
        if isBurning then creature.Damage 1

        if interactions >= 5 then isBurning <- false

    override this.FullyOccupy = false


    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "  ", Color.Red, Color.Red)


type FleshEatingPlant (startPosition) =
    inherit Item ()

    override this.Position = startPosition

    override this.InteractWith (creature: Creature) = creature.Damage 5

    override this.FullyOccupy = true

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "  ", Color.Green, Color.Green)



type Exit (startPosition) =
    inherit Item ()

    override this.Position = startPosition

    override this.InteractWith (creature: Creature) = 
        // Show end game notice
        System.Console.Clear ()
        printfn "You won!!!!"

    override this.FullyOccupy = false

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "  ", Color.Green, Color.Green)








// MARK: World

type World (canvas: Canvas, x:int, y:int) =
    let mutable _world: (Entity option * Item) [,] = Array2D.init x y (fun i j -> (None, (Grass (j, i) :> Item)))

    member this.world = _world

    member this.AddItem (item: Item, x:int, y:int) =
         _world.[y,x] <- (fst _world.[y,x], item)
         item.RenderOn canvas


    member this.Play () =

        let player = Player (20,50, Mage (),canvas, this.world)
        let enemy = Enemy (0, 0, canvas, player, this.world)

        player.RenderOn canvas
        enemy.RenderOn canvas
        canvas.Show (fst player.Position, snd player.Position)

        let mutable gameEnded = false
        while not gameEnded do

            while System.Console.KeyAvailable = false do
                let tempWorld = Array2D.copy _world
                for y = 0 to Array2D.length1 tempWorld - 1 do
                    for x = 0 to Array2D.length2 tempWorld - 1 do
                        let object = fst tempWorld.[y,x]
                        if object.IsSome then
                            if object.Value <> (player :> Entity) then
                                object.Value.Update ()



                // enemy.Update ()
                player.UpdateAttackCounter ()
                canvas.Show (fst player.Position, snd player.Position)
                System.Threading.Thread.Sleep(250)

            player.Update ()

            // if System.Console.KeyAvailable = true then
            //     player.Update (_world)

            // enemy.Update (_world)
            // canvas.Show (fst player.Position, snd player.Position)
            // System.Threading.Thread.Sleep(250)
            
            
 



            


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



    
