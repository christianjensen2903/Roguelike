// open System

type Color = System.ConsoleColor
type Direction = Left | Right | Down | Up
type Effect = Frozen | Poisoned
type GameState = Playing | Paused | GameOver | Starting
type Stat = Damage | Health | Spellpower | Armor | Speed | MagicResistance | Critchance | Critdamage

// Size of entire world
let worldSizeX = 100
let worldSizeY = 100
// Size of visible world
let screenSizeX = 50
let screenSizeY = 30

///<summary>Generates a random number</summary>
///<returns>Returns a random numer</returns>
let randomNumber (lower: int) (upper: int) =
    let random = new System.Random()
    random.Next(lower, upper)

///<summary>Calculates the distance betweeem two coordinates</summary>
///<input name="pos1">First coordinate</input>
///<input name="pos2">Second coordinate</input>
///<returns>Returns the distance as an integer</returns>
let getDistance (pos1: int*int) (pos2: int*int) =
    // Get positions
    let (x1, y1) = pos1
    let (x2, y2) = pos2

    // Calculate distance to player
    let dx = x1 - x2
    let dy = y1 - y2
    int (sqrt (float(dx)**2. + float(dy)**2.))
    
///<summary>Calculates the direction to a coordinate</summary>
///<input name="pos1">From coordinate</input>
///<input name="pos2">To coordinate</input>
///<returns>Returns the distance as an integer</returns>
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
///<summary>The canvas responsible for showing the player what is happening</summary>
///<input name="rows">Height of canvas</input>
///<input name="cols">Width of canvas</input>
type Canvas (rows: int, cols: int) =

    let mutable _screen = Array2D.create cols rows ("  ", Color.DarkGreen, Color.DarkGreen)
    let mutable _HUD: string = ""

    ///<summary>Gets the Color and text on given coordinate</summary>
    ///<input name="x">x-coordiante</input>
    ///<input name="y">y-coordinate</input>
    member this.Get (x:int, y:int) =
        _screen.[y,x]

    ///<summary>Sets the Color and text on given coordinate</summary>
    ///<input name="x">x-coordiante</input>
    ///<input name="y">y-coordinate</input>
    member this.Set (x: int, y: int, c: string, fg: Color, bg: Color) =
        _screen.[y,x] <- (c, bg, fg)

    ///<summary>Sets the Heads up display text</summary>
    ///<input name="text">The text</input>
    member this.SetHUD (text: string) = _HUD <- text


    ///<summary>Shows the menu</summary>
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
    
    ///<summary>Shows the map in a given radius around the player</summary>
    ///<input name="playerX">x-coordiante</input>
    ///<input name="playerY">y-coordinate</input>
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






///<summary>The entity interface with basic functions</summary>
and [<AbstractClass>] Entity () =
    abstract member RenderOn: Canvas -> unit
    default this.RenderOn (canvas: Canvas) = ()

    abstract member Update: unit -> unit
    default this.Update () = ()

    abstract member Icon: string
    default this.Icon = "  "

    


///<summary>Creature interface. Inherits from Entity</summary>
///<input name="x">x start position</input>
///<input name="y">y start position</input>
///<input name="canvas">The canvas. Gives a creature access to change the canvas</input>
///<input name="world">Gives the creature access to the world 2DArray. The array used to display the world</input>
and [<AbstractClass>] Creature (x:int, y:int, canvas: Canvas, world: (Entity option * Item) [,]) =
    inherit Entity ()

    let mutable _position = (x,y)
    let mutable _hitPoints = 1
    let mutable _isDead = false
    let mutable _target: Creature option = None
    let mutable _attackTimer = 0
    let mutable _effect: Effect option = None
    let mutable _effectTimer: int = 0
    let mutable _icon: string = ""
    let mutable _level: int = 1
    let mutable _outOfCombatTimer = 50

    ///<summary>A lot of getters and setters. Are pretty self explanatory</summary>
    member __.Position
        with get () = _position
        and set (value) = _position <- value

    member __.Level
        with get () = _level
        and set (value) = _level <- value

    member __.Icon
        with get () = _icon
        and set (value) = _icon <- value

    member __.OutOfCombatTimer
        with get () = _outOfCombatTimer
        and set (value) = _outOfCombatTimer <- value

    member __.AttackTimer
        with get () = _attackTimer
        and set (value) = _attackTimer <- value

    member __.EffectTimer
        with get () = _effectTimer
        and set (value) = _effectTimer <- value
    
    member __.Effect
        with get () = _effect
        and set (value) = _effect <- value

    member __.CheckOutOfCombat () =
        __.OutOfCombatTimer <- __.OutOfCombatTimer + 1
        if __.OutOfCombatTimer >= 50 then
            __.Heal (__.MaxHealth / 20 + 1)


    member __.HitPoints
        with get () = _hitPoints
        and set (value) = _hitPoints <- value

    member __.IsDead
        with get () = _isDead
        and set (value) = _isDead <- value

    member __.Damage (dmg: int) =
        _outOfCombatTimer <- 0
        _hitPoints <- _hitPoints - dmg

        if _hitPoints <= 0 then
            __.Die ()

    
    member __.Heal (h: int) =
        if _hitPoints + h > __.MaxHealth then
            _hitPoints <- __.MaxHealth
        else
            _hitPoints <- _hitPoints + h       
    

    member __.UpdateEffect () =
        if _effectTimer > 0 then 
            _effectTimer <- _effectTimer - 1
        else if _effectTimer = 0 then _effect <- None

    member __.UpdateAttackCounter () =
        if _attackTimer > 0 then _attackTimer <- _attackTimer - 1


    ///<summary> RenderOn Overwritten to suit class.</summary>
    ///<input name="canvas">The canvas</input>
    override __.RenderOn (canvas: Canvas) =
         let x,y = __.Position
         let _, fg, bg = canvas.Get (x,y)
         canvas.Set(x, y, _icon, fg, bg)

    ///<summary> Moves to coordinate if not obstructed</summary>
    ///<input name="x">The x-coordiante</input>
    ///<input name="y">The y-coordiante</input>
    member this.MoveTo (x: int, y: int) =
        let oldX,oldY = this.Position
        let _, fg, bg = canvas.Get (oldX,oldY)
        let field = world.[y,x]
        let item = snd field
        if not (fst field).IsSome && item.FullyOccupy = false then
            world.[oldY, oldX] <- (None, snd world.[oldY, oldX])
            (snd world.[oldY, oldX]).RenderOn canvas
            world.[y,x] <- (Some (this :> Entity), item)
            this.Position <- (x,y)
            item.InteractWith this
        else
            ()

    ///<summary>Variables and functions to be overritten by classes inheriting from this interface</summary>
    abstract member MaxHealth: int
    abstract member Attack: unit -> unit
    abstract member Die: unit -> unit

///<summary>The Item interface inheriting from Entity</summary>
and [<AbstractClass>] Item () =
    inherit Entity ()

    abstract member Position: (int * int)
    default this.Position = (0,0)

    // InteractWith. Gives access to change variables in a Creature
    abstract member InteractWith: Creature -> unit

    abstract member FullyOccupy: bool







///<summary>The Item interface inheriting from Entity</summary>
///<input name="startPosition">The startPosition coordinate</input>
///<input name="deficon">The icon of the projectile</input>
///<input name="deficon">The icon of the projectile showing which direction it is going in</input>
///<input name="dmg">The damage the projectile will deal</input>
///<input name="canvas">The canvas</input>
///<input name="world">The world 2DArray</input>
and Projectile (startPosition: (int * int), deficon: string, dirIcon: Map<Direction,string>, dmg: int, canvas: Canvas, world: (Entity option * Item) [,], direction: Direction) =
    inherit Item ()

    let mutable _removed = false
    let mutable _position = startPosition
    let mutable _icon = deficon
    let mutable _onHitEffect: Effect option = None

    member this.onHitEffect 
        with get () = _onHitEffect
        and set (value) = _onHitEffect <- value

    override this.Position = _position

    ///<summary>The interactWith overritten to suit this class</summary>
    ///<input name="creature">The creature to deal damage to</summary>
    override this.InteractWith (creature: Creature) =
        if _onHitEffect.IsSome then creature.EffectTimer <- 20
        if _onHitEffect.IsSome then creature.Effect <- _onHitEffect
        creature.Damage dmg

    override this.FullyOccupy = false

    // Same as the earlier ones
    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         let _, fg, bg = canvas.Get (x,y)
         canvas.Set(x, y, _icon, fg, bg)

    ///<summary>Removes the projectile from the canvas and world 2DArray</summary>
    member this.Remove () =
        let x, y = _position
        let _, fg, bg = canvas.Get (x,y)
        canvas.Set(x, y, "  ", fg, bg)
        world.[y, x] <- (None, snd world.[y, x])
        _removed <- true

           
    ///<summary>Updates the projectile. Moves it in a direction and removes it when it hits something</summary>
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
            
            // IsSome is true if the projectile hits ex. a player or enemy
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








///<summary>The spell Interface</summary>
and [<AbstractClass>] Spell () =
    let mutable _coolDownTimer = 0
    abstract member name: string
    abstract member baseDmg: int
    abstract member coolDown: int

    ///<summary>Calculates the damage the player can do to an enemy</summary>
    ///<input name="player">The player</input>
    member this.CalcDmg (player: Player) =
        let spellMultiplier =
            let rpgClass: RpgClass = player.RpgClass
            let mulOption: int option = rpgClass.statMultipliers.TryFind Stat.Spellpower
            if mulOption.IsSome then
                mulOption.Value
            else 1
        this.baseDmg * spellMultiplier 

    // Function to be overwritten
    abstract member Cast: Player * Enemy option * Canvas * (Entity option * Item) [,] -> unit
    default this.Cast (player: Player, target: Enemy option, canvas: Canvas, world: (Entity option * Item) [,]) = ()
    
    // Cooldown before this spell can be cast again
    member this.CoolDownTimer 
        with get () = _coolDownTimer
        and set (value) = _coolDownTimer <- value

    member this.UpdateTimer () =
        if _coolDownTimer > 0 then _coolDownTimer <- _coolDownTimer - 1


///<summary>The RapidFire spell inheriting from Spell interface</summary>
and RapidFire () =
    inherit Spell ()
    let mutable _castCount = 0

    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Rapid Fire"


    override this.Cast (player: Player, target: Enemy option, canvas: Canvas, world: (Entity option * Item) [,]) = 
        if target.IsSome && this.CoolDownTimer = 0 then
            player.Attack ()
            if _castCount = 2 then
                this.CoolDownTimer <- this.coolDown
                _castCount <- 0
            else 
                _castCount <- _castCount + 1
    


        
    
///<summary>Different types of spells with slight modifications</summary>

///<summary>Also freezes the target</summary>
and FreezingShot () =
    inherit Spell ()

    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Freezing Shot"

    override this.Cast (player: Player, target: Enemy option, canvas: Canvas, world: (Entity option * Item) [,]) =
        if target.IsSome && this.CoolDownTimer = 0 then
            this.CoolDownTimer <- this.coolDown
            let dis = getDistance target.Value.Position player.Position
            let dir = getDirection target.Value.Position player.Position
            let projectile = (Projectile ((fst player.Position, snd player.Position), "🔷",  Map.empty, this.CalcDmg (player), canvas, world, dir))
            projectile.onHitEffect <- Some Effect.Frozen
            projectile.Update ()

///<summary>Deals damage over time</summary>
and Poisonshot () =
    inherit Spell ()

    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Poison Shot"
    override this.Cast (player: Player, target: Enemy option, canvas: Canvas, world: (Entity option * Item) [,]) =
        if target.IsSome && this.CoolDownTimer = 0 then
            this.CoolDownTimer <- this.coolDown
            let dis = getDistance target.Value.Position player.Position
            let dir = getDirection target.Value.Position player.Position
            let projectile = (Projectile ((fst player.Position, snd player.Position), "🟢",  Map.empty, this.CalcDmg (player), canvas, world, dir))
            projectile.onHitEffect <- Some Effect.Poisoned
            projectile.Update ()

///<summary>Standard on-hit damage</summary>
and Fireblast () =
    inherit Spell ()

    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Fireblast"
    override this.Cast (player: Player, target: Enemy option, canvas: Canvas, world: (Entity option * Item) [,]) =
        if target.IsSome && this.CoolDownTimer = 0 then
            this.CoolDownTimer <- this.coolDown
            let dis = getDistance target.Value.Position player.Position
            let dir = getDirection target.Value.Position player.Position
            let projectile = (Projectile ((fst player.Position, snd player.Position), "🔥",  Map.empty, this.CalcDmg (player), canvas, world, dir))
            projectile.Update ()

///<summary>Freezes in a radius</summary>
and Freezenova () =
    inherit Spell ()

    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Freeze nova"

    override this.Cast (player: Player, target: Enemy option, canvas: Canvas, world: (Entity option * Item) [,]) =
        if this.CoolDownTimer = 0 then
            let nearbyEnemies: Enemy list = player.EnemiesWithin 5
            for i = 0 to List.length nearbyEnemies - 1 do
                nearbyEnemies.[i].EffectTimer <- 10
                nearbyEnemies.[i].Effect <- Some Effect.Frozen
            
///<summary>Standard on-hit damage</summary>
and Lightningbolt () =
    inherit Spell ()

    override this.baseDmg = 5
    override this.coolDown = 15
    override this.name = "Lightning bolt"

    override this.Cast (player: Player, target: Enemy option, canvas: Canvas, world: (Entity option * Item) [,]) =
        if target.IsSome && this.CoolDownTimer = 0 then
            this.CoolDownTimer <- this.coolDown
            let dis = getDistance target.Value.Position player.Position
            let dir = getDirection target.Value.Position player.Position
            let projectile = (Projectile ((fst player.Position, snd player.Position), "〽️",  Map.empty, this.CalcDmg (player), canvas, world, dir))
            projectile.Update ()











///<summary>The Inventory Item interface inheriting from Item</summary>
and [<AbstractClass>] InvItem () =
    inherit Item ()

    abstract member name: string

    abstract member icon: string
    
    abstract member stats: Map<string, int>

    override this.InteractWith (creature: Creature) =
        // Pickup item -> add to inv 
        ()

    override this.FullyOccupy = false

///<summary>The weapon class inheriting from the InvItem interface</summary>
and Weapon (name: string, icon: string, dmg: int, spellpower: int, speed: int) =
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






///<summary>The RpGClass interface</summary>
and [<AbstractClass>] RpgClass () =
    // The players startweapon
    let startWeapon: Weapon = Weapon ("Fist", "👊", 1,1,1)

    // The list of spells the RpgClass can choose from
    abstract member spells: Spell list

    // The bonus factors a RpgClass can have with certain abilities
    abstract member statMultipliers: Map<Stat, int>

///<summary>The Hunter RpgClass inheriting from the RpgClass Interface</summary>
and Hunter () =
    inherit RpgClass ()

    ///<summary>The spells a Hunter can cast</summary>
    let _spells: Spell list = 
        [RapidFire ();
        FreezingShot ();
        Poisonshot ()]
    
    override this.spells = _spells

    ///<summary>The multipliers a Hunter has</summary>
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

///<summary>The Warrior RpgClass inheriting from the RpgClass Interface</summary>
and Warrior () =
    inherit RpgClass ()

    ///<summary>The spells a Warrior can cast</summary>
    override this.spells =
        [RapidFire ();
        FreezingShot ();
        Poisonshot ()]

    ///<summary>The multipliers a Warrior has</summary>
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

///<summary>The Warrior RpgClass inheriting from the RpgClass Interface</summary>
and Mage () =
    inherit RpgClass ()

    ///<summary>The spells a Mage can cast</summary>
    override this.spells =
        [Fireblast ();
        Freezenova ();
        Lightningbolt ()]

    ///<summary>The multipliers a Mage has</summary>
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
///<summary>The player class inheriting from the Creature Interface</summary>
///<input name="x">The x spawn coordinate</input>
///<input name="y">The y spawn coordinate</input>
///<input name="rpgClass">The players RpgClass</input>
///<input name="canvas">The Canvas</input>
///<input name="world">The world 2DArray</input>
and Player (x:int, y:int, rpgClass: RpgClass, canvas: Canvas, world: (Entity option * Item) [,]) =
    inherit Creature (x,y, canvas, world)

    // The target the player will shoot/attack towards
    let mutable _target: Enemy option = None
    let mutable _attackTimer = 0
    let mutable _effect: Effect option = None
    let mutable _effectTimer: int = 0
    let mutable _xp: int = 0

    member this.XP = _xp
    member this.XPTarget = this.Level * 100

    override this.MaxHealth =
        let healthMultiplier =
            let mulOption: int option = rpgClass.statMultipliers.TryFind Stat.Health
            if mulOption.IsSome then
                mulOption.Value
            else 1

        this.Level * healthMultiplier * 10

    member this.RpgClass = rpgClass
    
    member this.Target = _target

    member this.GainXP amount =
        _xp <- amount + _xp
        if _xp >= this.XPTarget then
            this.Level <- this.Level + 1
            let remains = this.XPTarget - _xp
            _xp <- 0
            this.GainXP (remains)
            
    
    override __.Die () =
        __.IsDead <- true

    ///<summary>Finds all the enemies within a certain distance</summary>
    ///<input name="distance">The radius</input>
    ///<returns>A list of enemies</returns>
    member this.EnemiesWithin (distance: int) =
        // Checks if input is an enemy
        let withinDistance (elm: (Entity option * Item)) =
            let object = fst elm
            if _target.IsSome then _target.Value.RemoveTarget ()

            if object.IsSome then
                match object.Value with
                | :? Enemy ->
                    let enemy = object.Value :?> Enemy
                    if getDistance enemy.Position this.Position < distance then Some enemy else None

                | _ -> None
            else None

        // Looks at every world position and checks for enemies. For each position it calls "withinDistance"
        let mutable nearbyEnemies: Enemy list = []
        for x in [0..(Array2D.length1 world) - 1] do 
                for y in [0..(Array2D.length2 world) - 1] do
                    let enemy = withinDistance(world.[x, y])
                    if enemy.IsSome then nearbyEnemies <- nearbyEnemies @ [enemy.Value]
        nearbyEnemies

    ///<summary>Picks new target from the list of nearby enemies</summary>
    member this.SwitchTarget () =
        
        let nearbyEnemies = this.EnemiesWithin 20
        if List.length nearbyEnemies - 1 >= 0 then
            let target = nearbyEnemies.[randomNumber 0 (List.length nearbyEnemies - 1)]
            _target <- Some target
            target.Target ()
        else ()
    
    ///<summary>Attacks in the direction of the enemy</summary>
    override this.Attack () =

        let dis = getDistance _target.Value.Position this.Position
        let dir = getDirection _target.Value.Position this.Position

        match rpgClass with
        // If the player plays as a Hunter shoot the enemy
        | :? Hunter ->
            let icon = [Direction.Up, "⬆️ "; Direction.Down, "⬇️ "; Direction.Left, "⬅️ "; Direction.Right, " ⏩"] |> Map.ofList
            (Projectile ((fst this.Position, snd this.Position), "⏺ ",  icon, 2, canvas, world, dir)).Update ()

        // If the player plays as a Warrior hit the enemy
        | :? Warrior -> if dis < 2 then _target.Value.Damage 3

        // If the player plays as a Mage shoot the enemy
        | :? Mage -> (Projectile ((fst this.Position, snd this.Position), "🟠",  Map.empty, 2, canvas, world, dir)).Update ()
        
        | _ -> ()
        this.AttackTimer <- 5

    ///<summary>Sets icon based on RpgClass</summary>
    member this.SetIcon () =
        match rpgClass with
        | :? Hunter -> this.Icon <- "🧝"
        | :? Warrior -> this.Icon <- "🥷🏼"
        | :? Mage -> this.Icon <- "🧙"
        | _ -> this.Icon <- "👨"

    member this.UpdateSpellTimers () =
        List.iter (fun (elm: Spell) -> elm.UpdateTimer ()) rpgClass.spells

    
    ///<summary>Handles the different player inputs. Moving, shooting ex.</summary>
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
        | System.ConsoleKey.D1 -> rpgClass.spells.[0].Cast (this, _target, canvas, world)
        | System.ConsoleKey.D2 -> rpgClass.spells.[1].Cast (this, _target, canvas, world)
        | System.ConsoleKey.D3 -> rpgClass.spells.[2].Cast (this, _target, canvas, world)
        | _ -> ()

        this.MoveTo (x, y)

    ///<summary>Updates all cooldowns</summary>
    member this.UpdateCounters () =
        this.UpdateAttackCounter ()
        this.UpdateSpellTimers ()
        this.CheckOutOfCombat ()

    ///<summary>This happens every x second. Realtime updates</summary>
    override this.Update () =
        this.SetIcon ()
        this.HandleKeypress ()
        this.RenderOn (canvas)
        










// MARK: Enemy
///<summary>The Enemy class inheriting from the Creature Interface</summary>
///<input name="x">The x spawn coordinate</input>
///<input name="y">The y spawn coordinate</input>
///<input name="icon">The enemy icon</input>
///<input name="canvas">The Canvas</input>
///<input name="player">The player</input>
///<input name="world">The world 2DArray</input>
and Enemy (x:int, y:int, icon: string, canvas: Canvas, player: Player, world: (Entity option * Item) [,]) =
    inherit Creature (x,y, canvas, world)

    let mutable _spawnPoint = (x,y)
    let mutable _movingToSpawn = false
    let mutable _isTarget = false
    let mutable _spawnTimer = 0
    let mutable _effect: Effect option = None
    let mutable _effectTimer: int = 0


    
    override this.MaxHealth = player.Level * 10
    
    override __.Die () =
        let x, y = __.Position
        let item = snd world.[y,x]
        world.[y,x] <- (None, item)
        item.RenderOn canvas
        _spawnTimer <- 10
        __.IsDead <- true
        __.EffectTimer <- 0
        __.Effect <- None
        player.GainXP (__.Level * 10)


    member this.Target () = _isTarget <- true
    member this.RemoveTarget () = _isTarget <- false

    // Attacks the player if it is in range
    override this.Attack () = if getDistance this.Position player.Position <= 1 then player.Damage 1  
    
    ///<summary>Renders the enemy on the canvas. If it is a targeted enemy change the color</summary>
    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         let _, fg, bg = canvas.Get (x,y)
         world.[y,x] <- (Some (this :> Entity), snd world.[y,x])

         if _isTarget then
            canvas.Set(x, y, this.Icon, Color.Red, bg)
         else
            canvas.Set(x, y, this.Icon, fg, bg)

    ///<summary>Spawn enemy at This.Position</summary>
    member this.Spawn () =
        if _spawnTimer <= 0 then
            this.Position <- _spawnPoint
            let x, y = this.Position
            let item = snd world.[y,x]
            item.RenderOn canvas
            world.[y,x] <- (Some (this :> Entity), item)
            this.IsDead <- false
            this.HitPoints <- this.MaxHealth

        else 
            _spawnTimer <- _spawnTimer - 1

    ///<summary>Moves In given direction</summary>
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

    ///<summary>Moves towards spawn if distance to spawnpoint gets to big</summary>
    member this.MoveToSpawn() =
        let dis = getDistance _spawnPoint this.Position

        if dis < 3 then _movingToSpawn <- false

        if (dis > 30 || _movingToSpawn) then
            _movingToSpawn <- true
            this.MoveIn (getDirection _spawnPoint this.Position)
            
    ///<summary>Moves towards player if the player is within a certain distance</summary>
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

        

    ///<summary>Updates every x second. Checks for the enemy state.</summary>
    override this.Update () =
        if not this.IsDead then
            this.UpdateEffect ()
            this.CheckOutOfCombat ()
            match this.Effect with
            | Some Frozen ->
                this.Icon <- "🥶"
            | Some Poisoned ->
                this.Icon <- "🤢"
                this.Damage (1 * player.Level)
                this.MoveTowardsPlayer ()
                this.Attack ()
            | _ ->
                this.Icon <- icon
                this.MoveTowardsPlayer ()
                this.Attack ()

            if not this.IsDead then
                this.RenderOn canvas
        else this.Spawn ()







// MARK: World objects
///<summary>The grass class inheriting from the Item Interface</summary>
///<input name="startPosition">The grass positon in the world</input>
type Grass (startPosition) =
    inherit Item ()

    override this.Position = startPosition

    // Does nothing when a player walks on top of it
    override this.InteractWith (creature: Creature) = ()

    override this.FullyOccupy = false

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "  ", Color.DarkGreen, Color.DarkGreen)




///<summary>The Wall class inheriting from the Item Interface</summary>
///<input name="startPosition">The Wall positon in the world</input>
type Wall (startPosition) =
    inherit Item ()

    override this.Position = startPosition

    // Does nothing
    override this.InteractWith (creature: Creature) = ()

    // Cannot be walked over
    override this.FullyOccupy = true

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "  ", Color.Black, Color.Black)
    


///<summary>The Water class inheriting from the Item Interface</summary>
///<input name="startPosition">The Water positon in the world</input>
type Water (startPosition) =
    inherit Item ()

    override this.Position = startPosition

    // Heals the player
    override this.InteractWith (creature: Creature) = creature.Heal 2

    override this.FullyOccupy = false

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "  ", Color.DarkBlue, Color.DarkBlue)


///<summary>The Fire class inheriting from the Item Interface</summary>
///<input name="startPosition">The Fire positon in the world</input>
type Fire (startPosition) =
    inherit Item ()

    let mutable interactions = 0
    let mutable isBurning = true

    override this.Position = startPosition

    // Damages the player until it dies
    override this.InteractWith (creature: Creature) =
        if isBurning then 
            creature.Damage 1 
            interactions <- interactions + 1

        if interactions >= 5 then isBurning <- false

    override this.FullyOccupy = false


    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "  ", Color.Red, Color.Red)

///<summary>The FleshEatingPlant class inheriting from the Item Interface</summary>
///<input name="startPosition">The FleshEatingPlant positon in the world</input>
type FleshEatingPlant (startPosition) =
    inherit Item ()

    override this.Position = startPosition

    // Bites the player dealing damage
    override this.InteractWith (creature: Creature) = creature.Damage 5

    override this.FullyOccupy = true

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "  ", Color.Green, Color.Green)


///<summary>The Exit class inheriting from the Item Interface</summary>
///<input name="startPosition">The Exit positon in the world</input>
///<input name="world">The Exit positon in the world</input>
type Exit (startPosition, winFunction) =
    inherit Item ()

    override this.Position = startPosition

    // Wins the game
    override this.InteractWith (creature: Creature) = 
        winFunction()

    override this.FullyOccupy = false

    override this.RenderOn (canvas: Canvas) =
         let x,y = this.Position
         canvas.Set(x, y, "🚪", Color.Green, Color.Green)






// MARK: World
///<summary>The world class. Controls the game. The GameController/GameMaster</summary>
///<input name="x">x width</input>
///<input name="y">y width</input>
///<input name="canvas">The canvas</input>
type World (canvas: Canvas, x:int, y:int) =
    let mutable _world: (Entity option * Item) [,] = Array2D.init y x (fun i j -> (None, (Grass (j, i) :> Item)))
    let mutable _gameState: GameState = GameState.Playing
    let mutable _enemies: Enemy list = []

    let mutable player = Player (56,61,Hunter (), canvas, _world)
    

    member this.world = _world

    ///<summary>Adds an item to the world 2DArray</summary>
    ///<input name="item">The item to be placed</input>
    ///<input name="x">x coordinate</input>
    ///<input name="y">y coordinate</input>
    member this.AddItem (item: Item, x:int, y:int) =
         _world.[y,x] <- (fst _world.[y,x], item)
         item.RenderOn canvas

    member this.AddObject (object: Entity, x:int, y:int) =
         _world.[y,x] <- (Some object, snd _world.[y,x])
         object.RenderOn canvas

    
    member this.SetPlayer (p: Player) = 
        (x,y) = p.Position
        this.AddObject (p, 56, 61)
        player <- p

    ///<summary>Sets the heads up display showing the players stats</summary>
    ///<input name="player">The player</input>
    member this.SetHUD (player: Player) =
        let text = [
            "Player:                                     ";
            sprintf "HP: %A/%A                           " player.HitPoints player.MaxHealth;
            sprintf "Lvl: %A                             " player.Level;
            sprintf "XP: %A/%A                           " player.XP player.XPTarget;
            "Spells:                                     ";
            sprintf "1 - %s: %A                          " player.RpgClass.spells.[0].name player.RpgClass.spells.[0].CoolDownTimer;
            sprintf "2 - %s: %A                          " player.RpgClass.spells.[1].name player.RpgClass.spells.[1].CoolDownTimer;
            sprintf "3 - %s: %A                          " player.RpgClass.spells.[2].name player.RpgClass.spells.[2].CoolDownTimer;
            "\nTarget:                                   ";
            sprintf "%s                                  " (if player.Target.IsSome then sprintf "HP: %A/%A" player.Target.Value.HitPoints player.Target.Value.MaxHealth else "No enemy targeted");
            sprintf "%s                                  " (if player.Target.IsSome then sprintf "Lvl: %A" player.Target.Value.Level else "")] |> String.concat "\n"
            

        canvas.SetHUD text

    // Updates the dead enemies
    member this.DeadEnemiesKeeper () =

        let deadEnemies = List.filter (fun (x: Enemy) -> x.IsDead) _enemies
        for i in deadEnemies do i.Update()

    ///<summary>Builds an item from X,Y to X,Y</summary>
    ///<input name="buildNumber">Decides which item to build.</input>
    ///<input name="startPosition">Start x,y</input>
    ///<input name="endPosition">End x,y</input>
    member this.Build (buildNumber, startPosition, endPosition) =
        let startX, startY = fst startPosition, snd startPosition
        let endX, endY = fst endPosition, snd endPosition

        let fromX, toX = if startX <= endX then startX, endX else endX, startX
        let fromY, toY = if startY <= endY then startY, endY else endY, startY

        for y = fromY to toY do
            for x = fromX to toX do
                match buildNumber with
                | 0 -> this.AddItem(Wall (y,x), y, x)
                | 1 -> this.AddItem(Water (y,x), y, x)
                | 2 -> this.AddItem(Fire (y,x), y, x)
                | 3 -> this.AddItem(Grass (y,x), y, x)
                | _ -> this.AddItem(FleshEatingPlant (y,x), y, x)

    member this.win () =
        _gameState <- GameState.GameOver
        System.Console.Clear ()

    ///<summary>Instantiates player and enemies and start the game as well as updating the state of the game</summary>
    member this.Play () =

        // Instantiates enemies
        let enemy = Enemy (6, 6, "🧟",canvas, player, this.world)
        _enemies <- _enemies @ [enemy]

        // Renders the player and enemies on the canvas
        player.RenderOn canvas
        enemy.RenderOn canvas
        //canvas.Show (fst player.Position, snd player.Position)

        // Keeps updating every Movable object 
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

                if player.IsDead then _gameState <- GameOver
                this.SetHUD player
                System.Threading.Thread.Sleep(250)

            player.Update ()

    member this.AddEnemies () =
        // Zombie camp
        this.AddObject (Enemy (9,30, "🧟", canvas, player, _world), 9, 30)
        this.AddObject (Enemy (9,32, "🧟‍♂️", canvas, player, _world), 9, 30)
        this.AddObject (Enemy (12,31, "🧟", canvas, player, _world), 9, 30)
        this.AddObject (Enemy (8,34, "🧟", canvas, player, _world), 9, 30)

        // Vampire house
        this.AddObject (Enemy (59,27, "🧛", canvas, player, _world), 59, 27)
        this.AddObject (Enemy (60,36, "🧛", canvas, player, _world), 60, 36)
        this.AddObject (Enemy (62,36, "🦇", canvas, player, _world), 62, 36)

        // Wizard gathering
        this.AddObject (Enemy (45,54, "🧙", canvas, player, _world), 45, 54)
        this.AddObject (Enemy (45,60, "🧙", canvas, player, _world), 45, 60)
        this.AddObject (Enemy (50,54, "🧙", canvas, player, _world), 50, 54)
        this.AddObject (Enemy (50,60, "🧙", canvas, player, _world), 50, 60)

        // Grass field
        this.AddObject (Enemy (22,85, "🐅", canvas, player, _world), 22, 85)
        this.AddObject (Enemy (26,90, "🐅", canvas, player, _world), 26, 90)
        this.AddObject (Enemy (24,93, "🐅", canvas, player, _world), 24, 93)

        // Lake
        this.AddObject (Enemy (22,11, "🐊", canvas, player, _world), 22, 11)
        this.AddObject (Enemy (24,13, "🐊", canvas, player, _world), 24, 13)
        this.AddObject (Enemy (20,10, "🐢", canvas, player, _world), 20, 10)

        // Exit guards
        this.AddObject (Enemy (58,93, "💂", canvas, player, _world), 58, 93)
        this.AddObject (Enemy (58,93, "💂", canvas, player, _world), 56, 94)
        this.AddObject (Enemy (58,93, "💂", canvas, player, _world), 58, 92)
        this.AddObject (Enemy (58,93, "💂", canvas, player, _world), 54, 92)
        this.AddObject (Enemy (58,93, "💂", canvas, player, _world), 56, 93)
        

    
    member this.BuildWorld () =

        // Exit
        this.AddItem (Exit ((59,97), this.win), 59,97)

        // Drawing borders
        this.Build(0, (1,1), (1,worldSizeX-2))
        this.Build(0, (1,1), (worldSizeY-2, 1))
        this.Build(0, (worldSizeY-2, 1), (worldSizeY-2,worldSizeX-2))
        this.Build(0, (worldSizeY-2,worldSizeX-2), (1, worldSizeX-2))

        // Water stream
        this.Build(1, (12,0), (12, 0))
        this.Build(1, (12,2), (12, 15))
        this.Build(1, (8,15), (16, 24))

        // Wall
        this.Build(0, (20, 2), (21, 8))
        this.Build(0, (20, 12), (21, 24))
        this.Build(0, (20, 35), (21, 50))

        // Fire wall
        this.Build(2, (22, 2), (22, 8))
        this.Build(2, (23, 2), (23, 7))
        this.Build(2, (24, 2), (24, 6))
        this.Build(2, (25, 2), (25, 5))
        this.Build(2, (26, 2), (26, 4))
        this.Build(2, (27, 2), (27, 3))
        this.Build(2, (28, 2), (28, 2))

        // Plants
        this.Build(4, (28, 10), (28, 10))
        this.Build(4, (35, 15), (35, 15))
        this.Build(4, (40, 10), (40, 10))

        // Arena
        this.Build(0, (50, 40), (50, 46))
        this.Build(0, (51, 40), (51, 45))
        this.Build(0, (52, 40), (52, 44))
        this.Build(0, (53, 40), (53, 43))
        this.Build(0, (54, 40), (54, 42))
        this.Build(0, (55, 40), (55, 41))
        this.Build(0, (56, 40), (56, 40))

        this.Build(0, (59, 40), (59, 40))
        this.Build(0, (60, 40), (60, 41))
        this.Build(0, (61, 40), (61, 42))
        this.Build(0, (62, 40), (62, 43))
        this.Build(0, (63, 40), (63, 44))
        this.Build(0, (64, 40), (64, 45))
        this.Build(0, (65, 40), (65, 46))

        this.Build(0, (65, 49), (65, 55))
        this.Build(0, (64, 50), (64, 55))
        this.Build(0, (63, 51), (63, 55))
        this.Build(0, (62, 52), (62, 55))
        this.Build(0, (61, 53), (61, 55))
        this.Build(0, (60, 54), (60, 55))
        this.Build(0, (59, 55), (59, 55))

        this.Build(0, (50, 49), (50, 55))
        this.Build(0, (51, 50), (51, 55))
        this.Build(0, (52, 51), (52, 55))
        this.Build(0, (53, 52), (53, 55))
        this.Build(0, (54, 53), (54, 55))
        this.Build(0, (55, 54), (55, 55))
        this.Build(0, (56, 55), (56, 55))

        // Weapon challenge
        this.Build(2, (26, 26), (41, 41))
        // Plant part in challenge
        this.Build(4, (32, 31), (36, 35))
        // Water part in challenge
        this.Build(1, (33, 32), (35, 34))
        this.Build(1, (33, 32), (35, 34))
        this.Build(3, (34, 33), (34, 33))
        // Route1 in challenge
        this.Build(3, (34, 36), (34, 37))
        this.Build(3, (35, 37), (35, 39))
        this.Build(3, (35, 39), (33, 39))
        this.Build(3, (33, 40), (33, 42))
        // Route2 in challenge
        this.Build(3, (37, 34), (37, 34))
        this.Build(3, (37, 35), (39, 35))
        this.Build(3, (39, 35), (39, 33))
        this.Build(3, (40, 33), (42, 33))


        // Enemy base1
        this.Build(0, (40, 55), (20, 72))
        this.Build(3, (38, 57), (22, 70))
        this.Build(3, (29, 55), (31, 57))
        this.Build(0, (30,60), (32,62))
        this.Build(0, (38,62), (40,64))
        this.Build(0, (24,62), (26,64))
        this.Build(0, (28,67), (34,70))
        this.Build(3, (29,68), (33,70))
        this.Build(3, (31,67), (31,67))


        // Enemy base with exit
        this.Build(0, (60, 60), (worldSizeY-1, worldSizeX-1))
        this.Build(3, (62, 62), (worldSizeY-3, worldSizeX-3))
        this.Build(0, (worldSizeY-1, 90), (90, worldSizeX-3))
        this.Build(3, (worldSizeY-3, 91), (91, worldSizeX-4))
        this.Build(3, (95, 90), (95, 90))
            
            
            
 



            





let basicBow = Weapon ("Basic Bow", "🏹", 2, 1, 2)
let basicSword = Weapon ("Basic Sword", "🗡", 2, 1, 1)
let basicStaff = Weapon ("Basic Staff", "🪄", 1, 3, 1)







// MARK: Start Menu
///<summary>The startmenu where you start the game and pick you RpgClass</summary>
///<input name="canvas">The canvas</input>
type StartMenu (canvas: Canvas) =

    let mutable _selection = 0

    ///<summary>Draws the menu</summary>
    ///<input name="options">The options you can click on. USed to select your RpgClass</input>
    member this.DrawMenu (options: string list) =
       
        let padding = 1
        let optionsLength = List.length options
        for i in 0 .. optionsLength - 1 do
            let option = options.[i]
            let mutable x = screenSizeX / 2 - (String.length option) / 2 - 1
            let y = screenSizeY / 2 - optionsLength / 2 * (padding + 1) + i * (padding + 1)
            
            let stringToPrint =
                let optionLength = List.length (Seq.toList option)
                let mutable string = option
                for i in 0 .. optionLength / 2 do
                    string <- " " + string + " "
                string

            for char in Seq.toList stringToPrint do

                if _selection = i then
                    // If selection is the same as option make text another color
                    canvas.Set (x, y, string char, Color.DarkGreen, Color.Black) 
                else
                    canvas.Set (x, y, string char, Color.DarkGreen, Color.White)

                x <- x + 1
        
        canvas.ShowMenu ()

    ///<summary>Controls the menu. Handles the on click events</summary>
    ///<input name="options">The options you can click on. Used to select your RpgClass</input>
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
                match _selection with
                    | 0 -> this.StartGameWith (Warrior ())
                    | 1 -> this.StartGameWith (Hunter ())
                    | 2 -> this.StartGameWith (Mage ())
                    | _ -> ()
            | _ -> ()

            this.DrawMenu options
    
    ///<summary>The class screen where you can select your class</summary>
    member this.ClassScreen () =

        let classesOptions = ["Warrior"; "Hunter"; "Mage"]

        this.DrawMenu classesOptions

        this.ControlMenu classesOptions
      
    ///<summary>Instantiates the world and new canvas</summary>
    ///<input name="rpgClass">The class the player picked from the menu</input>
    member this.StartGameWith (rpgClass: RpgClass) =
        let newCanvas = Canvas (200,200)
        System.Console.Clear ()
        let world = World (newCanvas, worldSizeX, worldSizeY)
        world.BuildWorld ()
        let player = Player (56,61, rpgClass,newCanvas, world.world)
        world.SetPlayer player
        world.AddEnemies ()
        world.Play ()

System.Console.Clear ()
let canvas = Canvas (screenSizeX,screenSizeY)

let menu = StartMenu canvas

// Shows the class Screen as the first thing when the program starts
menu.ClassScreen ()
