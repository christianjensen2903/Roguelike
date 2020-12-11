
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
                System.Console.ForegroundColor <- fg
                System.Console.BackgroundColor <- bg
                System.Console.Write(c)
                System.Console.ResetColor()
            System.Console.Write("\n")

let test = Canvas (10,10)

test.Show ()

test.Set (3, 4, ' ', Color.White, Color.Blue)
// test.Set (2, 2, ' ', Color.White, Color.Blue)
// test.Set (3, 4, ' ', Color.White, Color.Red)

// // Tegner en lang streg når man shower for anden gang. Hvilket ikke er meningen
test.Show ()

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
type Object () =
    inherit Entity ()

    abstract member InteractWith: Player -> unit

    abstract member FullyOccupy: bool


[<AbstractClass>]
type InvItem () =
    inherit Object ()

    abstract member name: string

    abstract member icon: string
    
    abstract member stats: Map<string, int>

    override this.InteractWith (player: Player) =
        // Pickup item -> add to inv 
        ()

    override this.FullyOccupy = false


type MeleeWeapon (name: string, icon: string, dmg: int, spellpower: int, speed: int) =
    inherit InvItem ()

    override this.name = name

    override this.icon = icon

    override this.stats =
       [ "Dmg.", dmg;
          "Spell power", spellpower;
          "Speed", speed]
        |> Map.ofList






type Wall () =
    inherit Object ()

    override this.InteractWith (player: Player) = ()

    override this.FullyOccupy = true



type Water () =
    inherit Object ()

    override this.InteractWith (player: Player) = player.Heal 2

    override this.FullyOccupy = false


type Fire () =
    inherit Object ()

    let mutable interactions = 0
    let mutable isBurning = true

    override this.InteractWith (player: Player) =
        if isBurning then player.Damage 1

        if interactions >= 5 then isBurning <- false

    override this.FullyOccupy = false


type FleshEatingPlant () =
    inherit Object ()

    override this.InteractWith (player: Player) = player.Damage 5

    override this.FullyOccupy = true


type Exit () =
    inherit Object ()

    override this.InteractWith (player: Player) = 
        // Show end game notice
        System.Console.Clear ()
        printfn "You won!!!!"

    override this.FullyOccupy = false
