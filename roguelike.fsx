// open System

type Color = System.ConsoleColor
type Direction = Left | Right | Down | Up
type Effect = Frozen | Poisoned

let worldSizeX = 100
let worldSizeY = 200
let screenSizeX = 50
let screenSizeY = 30


let randomNumber (lower: int) (upper: int) =
    let random = new System.Random()
    random.Next(lower, upper)


let getDistance (pos1: int*int) (pos2: int*int) =
    // Get positions
    let (x1, y1) = pos1
    let (x2, y2) = pos2

    // Calculate distance to player
    let dx = x1 - x2
    let dy = y1 - y2
    int (sqrt (float(dx)**2. + float(dy)**2.))

let getDirection (pos1: int*int) (pos2: int*int) =
    // Get positions
    let (x1, y1) = pos1
    let (x2, y2) = pos2

    let dx = x1 - x2
    let dy = y1 - y2
    
    // Shoot in the direction with biggest difference in position
    if (abs dx > abs dy) then
        if (dx > 0) then
            Direction.Right
        else
            Direction.Left
    else
        if (dy > 0) then
            Direction.Down
        else
            Direction.Up







// MARK: Canvas

type Canvas (rows: int, cols: int) =

    let mutable _screen = Array2D.create cols rows ("  ", Color.DarkGreen, Color.DarkGreen)
    let mutable _HUD: string = ""


    member this.Get (x:int, y:int) =
        _screen.[y,x]

    member this.Set (x: int, y: int, c: string, fg: Color, bg: Color) =
        _screen.[y,x] <- (c, bg, fg)

    member this.SetHUD (text: string) = _HUD <- text



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
        // System.Console.Clear ()
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

        printfn "%s" _HUD







and [<AbstractClass>] Entity () =

    let mutable _entities: Entity list = []
    abstract member RenderOn: Canvas -> unit
    default this.RenderOn (canvas: Canvas) = ()

    abstract member Update: unit -> unit
    default this.Update () = ()

    abstract member Icon: string
    default this.Icon = "  "

    member __.AddEntity entity = _entities <- entity :: _entities

    member __.AddEntitiesTo canvas = List.iter (fun (e: Entity) -> e.RenderOn canvas) _entities 

    



and [<AbstractClass>] Creature (x:int, y:int, canvas: Canvas, world: (Entity option * Item) [,]) =
    inherit Entity ()

    let mutable _position = (x,y)
    let mutable _maxHealth = 10
    let mutable _hitPoints = _maxHealth
    let mutable _isDead = false
    let mutable _target: Creature option = None
    let mutable _attackTimer = 0
    let mutable _effect: Effect option = None
    let mutable _effectTimer: int = 0
    let mutable _icon: string = ""

    member __.Position
        with get () = _position
        and set (value) = _position <- value

    member __.Icon
        with get () = _icon
        and set (value) = _icon <- value

    member __.AttackTimer
        with get () = _attackTimer
        and set (value) = _attackTimer <- value

    member __.EffectTimer
        with get () = _effectTimer
        and set (value) = _effectTimer <- value
    
    member __.Effect
        with get () = _effect
        and set (value) = _effect <- value

    member __.HitPoints
        with get () = _hitPoints
        and set (value) = _hitPoints <- value

    member __.IsDead
        with get () = _isDead
        and set (value) = _isDead <- value

    member __.Damage (dmg: int) =
        _hitPoints <- _hitPoints - dmg

        if _hitPoints <= 0 then
            __.Die ()

    
    member __.Heal (h: int) =
        _hitPoints <- _hitPoints + h       
    

    member __.UpdateEffect () =
        if _effectTimer > 0 then 
            _effectTimer <- _effectTimer - 1
        else if _effectTimer = 0 then _effect <- None

    member __.UpdateAttackCounter () =
        if _attackTimer > 0 then _attackTimer <- _attackTimer - 1


    override __.RenderOn (canvas: Canvas) =
         let x,y = __.Position
         let _, fg, bg = canvas.Get (x,y)
         canvas.Set(x, y, _icon, fg, bg)

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


    abstract member Attack: unit -> unit

    abstract member Die: unit -> unit
    


and [<AbstractClass>] Item () =
    inherit Entity ()

    abstract member Position: (int * int)
    default this.Position = (0,0)

    abstract member InteractWith: Creature -> unit

    abstract member FullyOccupy: bool








type Projectile (startPosition: (int * int), deficon: string, dirIcon: Map<Direction,string>, dmg: int, canvas: Canvas, world: (Entity option * Item) [,], direction: Direction) =
    inherit Item ()

    let mutable _removed = false
    let mutable _position = startPosition
    let mutable _icon = deficon
    let mutable _onHitEffect: Effect option = None

    member this.onHitEffect 
        with get () = _onHitEffect
        and set (value) = _onHitEffect <- value

    override this.Position = _position

    override this.InteractWith (creature: Creature) =
        if _onHitEffect.IsSome then creature.EffectTimer <- 20
        creature.Effect <- _onHitEffect
        creature.Damage dmg

    override this.FullyOccupy = false

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         let _, fg, bg = canvas.Get (x,y)
         canvas.Set(x, y, _icon, fg, bg)

    member this.Remove () =
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
            | Up when oldY > 0 -> 
                if (dirIcon.TryFind Up).IsSome then _icon <- (dirIcon.TryFind Up).Value
                newY <- oldY - 1
            | Down when oldY < worldSizeY - 2 ->
                if (dirIcon.TryFind Down).IsSome then _icon <- (dirIcon.TryFind Down).Value
                newY <- oldY + 1
            | Left when oldX > 0 ->
                if (dirIcon.TryFind Left).IsSome then _icon <- (dirIcon.TryFind Left).Value
                newX <- oldX - 1
            | Right when oldX < worldSizeX - 2 ->
                if (dirIcon.TryFind Right).IsSome then _icon <- (dirIcon.TryFind Up).Value
                newX <- oldX + 1
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








[<AbstractClass>]
type Spell () =
    abstract member name: string
    abstract member baseDmg: int
    abstract member coolDown: int
    abstract member coolDownTimer: int

    abstract member Cast: Creature * Creature option * Canvas * (Entity option * Item) [,] -> unit
    default this.Cast (player: Creature, target: Creature option, canvas: Canvas, world: (Entity option * Item) [,]) = ()

    abstract member UpdateTimer: unit -> unit
    default this.UpdateTimer () = ()



type RapidFire () =
    inherit Spell ()
    let mutable _castCount = 0
    let mutable _coolDownTimer = 0

    override this.coolDownTimer = _coolDownTimer
    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Rapid Fire"


    override this.Cast (player: Creature, target: Creature option, canvas: Canvas, world: (Entity option * Item) [,]) = 
        if target.IsSome && _coolDownTimer = 0 then
            player.Attack ()
            if _castCount = 2 then
                _coolDownTimer <- this.coolDown
                _castCount <- 0
            else 
                _castCount <- _castCount + 1
    
    override this.UpdateTimer () =
        if _coolDownTimer > 0 then _coolDownTimer <- _coolDownTimer - 1


        
    

type FreezingShot () =
    inherit Spell ()
    let mutable _coolDownTimer = 0

    override this.coolDownTimer = _coolDownTimer
    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Freezing Shot"

    override this.Cast (player: Creature, target: Creature option, canvas: Canvas, world: (Entity option * Item) [,]) =
        if target.IsSome then
            let dis = getDistance target.Value.Position player.Position
            let dir = getDirection target.Value.Position player.Position
            let projectile = (Projectile ((fst player.Position, snd player.Position), "🔷",  Map.empty, 2, canvas, world, dir))
            projectile.onHitEffect <- Some Effect.Frozen
            projectile.Update ()


type Poisonshot () =
    inherit Spell ()
    let mutable _coolDownTimer = 0

    override this.coolDownTimer = _coolDownTimer
    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Poison Shot"
    override this.Cast (player: Creature, target: Creature option, canvas: Canvas, world: (Entity option * Item) [,]) =
        if target.IsSome then
            let dis = getDistance target.Value.Position player.Position
            let dir = getDirection target.Value.Position player.Position
            let projectile = (Projectile ((fst player.Position, snd player.Position), "🟢",  Map.empty, 2, canvas, world, dir))
            projectile.onHitEffect <- Some Effect.Poisoned
            projectile.Update ()

type Fireblast () =
    inherit Spell ()
    let mutable _coolDownTimer = 0

    override this.coolDownTimer = _coolDownTimer
    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Fireblast"
    override this.Cast (player: Creature, target: Creature option, canvas: Canvas, world: (Entity option * Item) [,]) =
        if target.IsSome then
            let dis = getDistance target.Value.Position player.Position
            let dir = getDirection target.Value.Position player.Position
            let projectile = (Projectile ((fst player.Position, snd player.Position), "🔥",  Map.empty, 2, canvas, world, dir))
            projectile.onHitEffect <- Some Effect.Poisoned
            projectile.Update ()

type Freezenova () =
    inherit Spell ()
    let mutable _coolDownTimer = 0

    override this.coolDownTimer = _coolDownTimer
    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Freeze nova"

type Lightningbolt () =
    inherit Spell ()
    let mutable _coolDownTimer = 0

    override this.coolDownTimer = _coolDownTimer
    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Lightning bolt"











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



type GameState = Playing | Paused | GameOver | Starting


type Stat = Damage | Health | Spellpower | Armor | Speed | MagicResistance | Critchance | Critdamage

[<AbstractClass>]
type RpgClass () =
    abstract member startingWeapon: Weapon

    abstract member spells: Spell list

    abstract member statMultipliers: Map<Stat, int>


type Hunter () =
    inherit RpgClass ()
    

    let _spells: Spell list = 
        [RapidFire ();
        FreezingShot ();
        Poisonshot ()]
    

    override this.spells = _spells

    override this.startingWeapon = basicBow

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
        [RapidFire ();
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


            

















// MARK: Player

type Player (x:int, y:int, rpgClass: RpgClass, canvas: Canvas, world: (Entity option * Item) [,]) =
    inherit Creature (x,y, canvas, world)

    let mutable _target: Enemy option = None
    
    member this.Target = _target

    member this.RpgClass = rpgClass
    
    override __.Die () =
        __.IsDead <- true

    member this.SwitchTarget () =
        let withinDistance (elm: (Entity option * Item)) =
            let object = fst elm
            if _target.IsSome then _target.Value.RemoveTarget ()

            if object.IsSome then
                match object.Value with
                | :? Enemy ->
                    let enemy = object.Value :?> Enemy
                    if getDistance enemy.Position this.Position < 20 then Some enemy else None

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
    
    override this.Attack () =

        let dis = getDistance _target.Value.Position this.Position
        let dir = getDirection _target.Value.Position this.Position

        match rpgClass with
        | :? Hunter ->
            let icon = [Direction.Up, "⬆️ "; Direction.Down, "⬇️ "; Direction.Left, "⬅️ "; Direction.Right, " ⏩"] |> Map.ofList
            (Projectile ((fst this.Position, snd this.Position), "⏺ ",  icon, 2, canvas, world, dir)).Update ()

        | :? Warrior -> if dis < 2 then _target.Value.Damage 3

        | :? Mage -> (Projectile ((fst this.Position, snd this.Position), "🟠",  Map.empty, 2, canvas, world, dir)).Update ()
        
        | _ -> ()
        this.AttackTimer <- 5

    override this.Icon =
        match rpgClass with
        | :? Hunter -> "🧝🏼‍♀️"
        | :? Warrior -> "🥷🏼"
        | :? Mage -> "🧙🏼‍♂️"
        | _ -> "👨🏼‍💼"

    member this.UpdateSpellTimers () =
        List.iter (fun (elm: Spell) -> elm.UpdateTimer ()) rpgClass.spells

    

    member this.HandleKeypress () =
        let mutable x, y = this.Position
        let key = System.Console.ReadKey()
        
        match key.Key with
        | System.ConsoleKey.UpArrow when y > 0 -> y <- y - 1
        | System.ConsoleKey.DownArrow when y < worldSizeY - 1 -> y <- y + 1
        | System.ConsoleKey.LeftArrow when x > 0 -> x <- x - 1
        | System.ConsoleKey.RightArrow when x < worldSizeX - 1 -> x <- x + 1
        | System.ConsoleKey.Spacebar when _target.IsSome -> if this.AttackTimer = 0 then this.Attack ()
        | System.ConsoleKey.Tab -> this.SwitchTarget ()
        | System.ConsoleKey.D1 when _target.IsSome -> rpgClass.spells.[0].Cast (this :> Creature, Some (_target.Value :> Creature), canvas, world)
        | System.ConsoleKey.D2 when _target.IsSome -> rpgClass.spells.[1].Cast (this :> Creature, Some (_target.Value :> Creature), canvas, world)
        | System.ConsoleKey.D3 when _target.IsSome -> rpgClass.spells.[2].Cast (this :> Creature, Some (_target.Value :> Creature), canvas, world)
        | _ -> ()

        this.MoveTo (x, y)

    member this.UpdateCounters () =
        this.UpdateAttackCounter ()
        this.UpdateSpellTimers ()

    override this.Update () =
        this.HandleKeypress ()
        this.RenderOn (canvas)
        










// MARK: Enemy
and Enemy (x:int, y:int, icon: string, canvas: Canvas, player: Player, world: (Entity option * Item) [,]) =
    inherit Creature (x,y, canvas, world)

    let mutable _spawnPoint = (x,y)
    let mutable _movingToSpawn = false
    let mutable _isTarget = false
    let mutable _spawnTimer = 0

    
    override __.Die () =
        let x, y = __.Position
        let item = snd world.[y,x]
        item.RenderOn canvas
        world.[y,x] <- (None, item)
        _spawnTimer <- 10
        __.IsDead <- true


    member this.Target () = _isTarget <- true
    member this.RemoveTarget () = _isTarget <- false

    override this.Attack () = if getDistance this.Position player.Position <= 1 then player.Damage 1  

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         let _, fg, bg = canvas.Get (x,y)
         world.[y,x] <- (Some (this :> Entity), snd world.[y,x])

         if _isTarget then
            canvas.Set(x, y, this.Icon, Color.Red, bg)
         else
            canvas.Set(x, y, this.Icon, fg, bg)

    member this.Spawn () =
        if _spawnTimer <= 0 then
            this.Position <- _spawnPoint
            let x, y = this.Position
            let item = snd world.[y,x]
            item.RenderOn canvas
            world.[y,x] <- (Some (this :> Entity), item)
            this.IsDead <- false

        else 
            _spawnTimer <- _spawnTimer - 1

    member this.MoveIn (direction: Direction) =
        
        let oldX,oldY = this.Position
        let mutable newX, newY = oldX, oldY
        let _, fg, bg = canvas.Get (oldX,oldY)

        match direction with
        | Direction.Up when oldY > 0 -> newY <- oldY - 1
        | Direction.Down when oldY < worldSizeY - 2 -> newY <- oldY + 1
        | Direction.Left when oldX > 0 -> newX <- oldX - 1
        | Direction.Right when oldX < worldSizeX - 2 -> newX <- oldX + 1
        | _ -> ()
        
        this.MoveTo (newX, newY)

    member this.MoveToSpawn() =
        let dis = getDistance _spawnPoint this.Position

        if dis < 3 then _movingToSpawn <- false

        if (dis > 30 || _movingToSpawn) then
            _movingToSpawn <- true
            this.MoveIn (getDirection _spawnPoint this.Position)
            

    member this.MoveTowardsPlayer () =
        let dis = getDistance player.Position this.Position
        let directions = [Direction.Left; Direction.Right; Direction.Up; Direction.Down]

        // Check if it should move to spawn
        this.MoveToSpawn ()

        // else move
        if (dis > 10) then  
            // If player are to far away move random direction
            let dirInd = randomNumber 0 (List.length directions)
            this.MoveIn directions.[dirInd]
        
        else if (_movingToSpawn = false) then
            this.MoveIn (getDirection player.Position this.Position)

        


    override this.Update () =
        if not this.IsDead then
            this.UpdateEffect ()
            match this.Effect with
            | Some Frozen ->
                this.Icon <- "🥶"
                ()
            | Some Poisoned ->
                this.Icon <- "🤢"
                this.Damage 5
                this.MoveTowardsPlayer ()
                this.Attack ()
            | _ ->
                this.Icon <- icon
                this.MoveTowardsPlayer ()
                this.Attack ()

            this.RenderOn canvas
        else this.Spawn ()







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
    let mutable _gameState: GameState = GameState.Playing
    let mutable _enemies: Enemy list = []

    

    member this.world = _world

    member this.AddItem (item: Item, x:int, y:int) =
         _world.[y,x] <- (fst _world.[y,x], item)
         item.RenderOn canvas

    member this.StateKeeper () =
        match _gameState with
        | Playing -> this.Play()
        | Paused -> ()
        | Starting -> ()
        | GameOver -> ()

    member this.SetHUD (player: Player) =
        let text = [
            "Player:                                     ";
            sprintf "HP: %A                              " player.HitPoints;
            "Spells:                                     ";
            sprintf "1 - %s: %A                          " player.RpgClass.spells.[0].name player.RpgClass.spells.[0].coolDownTimer;
            sprintf "2 - %s: %A                          " player.RpgClass.spells.[1].name player.RpgClass.spells.[1].coolDownTimer;
            sprintf "3 - %s: %A                          " player.RpgClass.spells.[2].name player.RpgClass.spells.[2].coolDownTimer;
            "Target:                                     ";
            sprintf "%s                                  " (if player.Target.IsSome then sprintf "HP: %A" player.Target.Value.HitPoints else "No enemy targeted")] |> String.concat "\n"
            // "test"
            
            // 
        //     player.HitPoints
        //     player.RpgClass.spells.[0].name player.RpgClass.spells.[0].coolDownTimer
        //     player.RpgClass.spells.[1].name player.RpgClass.spells.[1].coolDownTimer
        //     player.RpgClass.spells.[2].name player.RpgClass.spells.[2].coolDownTimer
            

        canvas.SetHUD text

    member this.DeadEnemiesKeeper () =

        let deadEnemies = List.filter (fun (x: Enemy) -> x.IsDead) _enemies
        for i in deadEnemies do i.Update()

    member this.Play () =

        let player = Player (12,10, Hunter (),canvas, this.world)
        let enemy = Enemy (0, 0, "🧟‍♀️",canvas, player, this.world)
        _enemies <- _enemies @ [enemy]

        player.RenderOn canvas
        enemy.RenderOn canvas
        canvas.Show (fst player.Position, snd player.Position)

        while _gameState = GameState.Playing do

            while System.Console.KeyAvailable = false do
                let tempWorld = Array2D.copy _world
                for y = 0 to Array2D.length1 tempWorld - 1 do
                    for x = 0 to Array2D.length2 tempWorld - 1 do
                        let object = fst tempWorld.[y,x]
                        if object.IsSome then
                            if object.Value <> (player :> Entity) then
                                object.Value.Update ()


                this.DeadEnemiesKeeper ()
                player.UpdateCounters ()

                canvas.Show (fst player.Position, snd player.Position)
                // if player.Target.IsSome then
                //     canvas.ShowHUD (player, Some (player.Target.Value :> Creature))
                // else canvas.ShowHUD (player, None)
                if player.IsDead then _gameState <- GameOver
                this.SetHUD player
                System.Threading.Thread.Sleep(250)

            player.Update ()
            
            
            
 



            


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