module Roguelike
type Color = System.ConsoleColor
type Direction =
  | Left
  | Right
  | Down
  | Up
type Effect =
  | Frozen
  | Poisoned
type GameState =
  | Playing
  | Paused
  | GameOver
  | Starting
type Stat =
  | Damage
  | Health
  | Spellpower
  | Armor
  | Speed
  | MagicResistance
  | Critchance
  | Critdamage
val worldSizeX : int
val worldSizeY : int
val screenSizeX : int
val screenSizeY : int
val randomNumber : lower:int -> upper:int -> int
val getDistance : int * int -> int * int -> int
val getDirection : int * int -> int * int -> Direction
type Canvas =
  class
    new : rows:int * cols:int -> Canvas
    member
      Get : x:int * y:int -> string * System.ConsoleColor * System.ConsoleColor
    member Set : x:int * y:int * c:string * fg:Color * bg:Color -> unit
    member SetHUD : text:string -> unit
    member Show : playerX:int * playerY:int -> unit
    member ShowMenu : unit -> unit
  end
[<AbstractClassAttribute ()>]
and Entity =
  class
    new : unit -> Entity
    abstract member RenderOn : Canvas -> unit
    abstract member Update : unit -> unit
    abstract member Icon : string
    override RenderOn : canvas:Canvas -> unit
    override Update : unit -> unit
    override Icon : string
  end
[<AbstractClassAttribute ()>]
and Creature =
  class
    inherit Entity
    new : x:int * y:int * canvas:Canvas * world:(Entity option * Item) [,] ->
            Creature
    abstract member Attack : unit -> unit
    abstract member Die : unit -> unit
    abstract member MaxHealth : int
    member CheckOutOfCombat : unit -> unit
    member Damage : dmg:int -> unit
    member Heal : h:int -> unit
    member MoveTo : x:int * y:int -> unit
    override RenderOn : canvas:Canvas -> unit
    member UpdateAttackCounter : unit -> unit
    member UpdateEffect : unit -> unit
    member AttackTimer : int
    member Effect : Effect option
    member EffectTimer : int
    member HitPoints : int
    member Icon : string
    member IsDead : bool
    member Level : int
    member OutOfCombatTimer : int
    member Position : int * int
    member AttackTimer : int with set
    member Effect : Effect option with set
    member EffectTimer : int with set
    member HitPoints : int with set
    member Icon : string with set
    member IsDead : bool with set
    member Level : int with set
    member OutOfCombatTimer : int with set
    member Position : int * int with set
  end
[<AbstractClassAttribute ()>]
and Item =
  class
    inherit Entity
    new : unit -> Item
    abstract member InteractWith : Creature -> unit
    abstract member FullyOccupy : bool
    abstract member Position : int * int
    override Position : int * int
  end
and Projectile =
  class
    inherit Item
    new : startPosition:(int * int) * deficon:string *
          dirIcon:Map<Direction,string> * dmg:int * canvas:Canvas *
          world:(Entity option * Item) [,] * direction:Direction -> Projectile
    override InteractWith : creature:Creature -> unit
    member Remove : unit -> unit
    override RenderOn : canvas:Canvas -> unit
    override Update : unit -> unit
    override FullyOccupy : bool
    override Position : int * int
    member onHitEffect : Effect option
    member onHitEffect : Effect option with set
  end
[<AbstractClassAttribute ()>]
and Spell =
  class
    new : unit -> Spell
    abstract member
      Cast : Player * Enemy option * Canvas * (Entity option * Item) [,] -> unit
    abstract member baseDmg : int
    abstract member coolDown : int
    abstract member name : string
    member CalcDmg : player:Player -> int
    override
      Cast : player:Player * target:Enemy option * canvas:Canvas *
             world:(Entity option * Item) [,] -> unit
    member UpdateTimer : unit -> unit
    member CoolDownTimer : int
    member CoolDownTimer : int with set
  end
and RapidFire =
  class
    inherit Spell
    new : unit -> RapidFire
    override
      Cast : player:Player * target:Enemy option * canvas:Canvas *
             world:(Entity option * Item) [,] -> unit
    override baseDmg : int
    override coolDown : int
    override name : string
  end
and FreezingShot =
  class
    inherit Spell
    new : unit -> FreezingShot
    override
      Cast : player:Player * target:Enemy option * canvas:Canvas *
             world:(Entity option * Item) [,] -> unit
    override baseDmg : int
    override coolDown : int
    override name : string
  end
and Poisonshot =
  class
    inherit Spell
    new : unit -> Poisonshot
    override
      Cast : player:Player * target:Enemy option * canvas:Canvas *
             world:(Entity option * Item) [,] -> unit
    override baseDmg : int
    override coolDown : int
    override name : string
  end
and Fireblast =
  class
    inherit Spell
    new : unit -> Fireblast
    override
      Cast : player:Player * target:Enemy option * canvas:Canvas *
             world:(Entity option * Item) [,] -> unit
    override baseDmg : int
    override coolDown : int
    override name : string
  end
and Freezenova =
  class
    inherit Spell
    new : unit -> Freezenova
    override
      Cast : player:Player * target:Enemy option * canvas:Canvas *
             world:(Entity option * Item) [,] -> unit
    override baseDmg : int
    override coolDown : int
    override name : string
  end
and Lightningbolt =
  class
    inherit Spell
    new : unit -> Lightningbolt
    override
      Cast : player:Player * target:Enemy option * canvas:Canvas *
             world:(Entity option * Item) [,] -> unit
    override baseDmg : int
    override coolDown : int
    override name : string
  end
[<AbstractClassAttribute ()>]
and InvItem =
  class
    inherit Item
    new : unit -> InvItem
    abstract member icon : string
    abstract member name : string
    abstract member stats : Map<string,int>
    override InteractWith : creature:Creature -> unit
    override FullyOccupy : bool
  end
and Weapon =
  class
    inherit InvItem
    new : name:string * icon:string * dmg:int * spellpower:int * speed:int ->
            Weapon
    override Position : int * int
    override icon : string
    override name : string
    override stats : Map<string,int>
  end
[<AbstractClassAttribute ()>]
and RpgClass =
  class
    new : unit -> RpgClass
    abstract member spells : Spell list
    abstract member statMultipliers : Map<Stat,int>
  end
and Hunter =
  class
    inherit RpgClass
    new : unit -> Hunter
    override spells : Spell list
    override statMultipliers : Map<Stat,int>
  end
and Warrior =
  class
    inherit RpgClass
    new : unit -> Warrior
    override spells : Spell list
    override statMultipliers : Map<Stat,int>
  end
and Mage =
  class
    inherit RpgClass
    new : unit -> Mage
    override spells : Spell list
    override statMultipliers : Map<Stat,int>
  end
and Player =
  class
    inherit Creature
    new : x:int * y:int * rpgClass:RpgClass * canvas:Canvas *
          world:(Entity option * Item) [,] -> Player
    override Attack : unit -> unit
    override Die : unit -> unit
    member EnemiesWithin : distance:int -> Enemy list
    member GainXP : amount:int -> unit
    member HandleKeypress : unit -> unit
    member SetIcon : unit -> unit
    member SwitchTarget : unit -> unit
    override Update : unit -> unit
    member UpdateCounters : unit -> unit
    member UpdateSpellTimers : unit -> unit
    member LevelUp : unit
    override MaxHealth : int
    member RpgClass : RpgClass
    member Target : Enemy option
    member XP : int
    member XPTarget : int
  end
and Enemy =
  class
    inherit Creature
    new : x:int * y:int * icon:string * canvas:Canvas * player:Player *
          world:(Entity option * Item) [,] -> Enemy
    override Attack : unit -> unit
    override Die : unit -> unit
    member MoveIn : direction:Direction -> unit
    member MoveToSpawn : unit -> unit
    member MoveTowardsPlayer : unit -> unit
    member RemoveTarget : unit -> unit
    override RenderOn : canvas:Canvas -> unit
    member Spawn : unit -> unit
    member Target : unit -> unit
    override Update : unit -> unit
    override MaxHealth : int
  end
type Grass =
  class
    inherit Item
    new : startPosition:(int * int) -> Grass
    override InteractWith : creature:Creature -> unit
    override RenderOn : canvas:Canvas -> unit
    override FullyOccupy : bool
    override Position : int * int
  end
type Wall =
  class
    inherit Item
    new : startPosition:(int * int) -> Wall
    override InteractWith : creature:Creature -> unit
    override RenderOn : canvas:Canvas -> unit
    override FullyOccupy : bool
    override Position : int * int
  end
type Water =
  class
    inherit Item
    new : startPosition:(int * int) -> Water
    override InteractWith : creature:Creature -> unit
    override RenderOn : canvas:Canvas -> unit
    override FullyOccupy : bool
    override Position : int * int
  end
type Fire =
  class
    inherit Item
    new : startPosition:(int * int) -> Fire
    override InteractWith : creature:Creature -> unit
    override RenderOn : canvas:Canvas -> unit
    override FullyOccupy : bool
    override Position : int * int
  end
type FleshEatingPlant =
  class
    inherit Item
    new : startPosition:(int * int) -> FleshEatingPlant
    override InteractWith : creature:Creature -> unit
    override RenderOn : canvas:Canvas -> unit
    override FullyOccupy : bool
    override Position : int * int
  end
type Exit =
  class
    inherit Item
    new : startPosition:(int * int) -> Exit
    override InteractWith : creature:Creature -> unit
    override RenderOn : canvas:Canvas -> unit
    override FullyOccupy : bool
    override Position : int * int
  end
type World =
  class
    new : canvas:Canvas * x:int * y:int -> World
    member AddEnemies : unit -> unit
    member AddItem : item:Item * x:int * y:int -> unit
    member AddObject : object:Entity * x:int * y:int -> unit
    member
      Build : buildNumber:int * startPosition:(int * int) *
              endPosition:(int * int) -> unit
    member BuildWorld : unit -> unit
    member DeadEnemiesKeeper : unit -> unit
    member Play : unit -> unit
    member SetHUD : player:Player -> unit
    member SetPlayer : p:Player -> unit
    member world : (Entity option * Item) [,]
  end
val basicBow : Weapon
val basicSword : Weapon
val basicStaff : Weapon
type StartMenu =
  class
    new : canvas:Canvas -> StartMenu
    member ClassScreen : unit -> unit
    member ControlMenu : options:string list -> unit
    member DrawMenu : options:string list -> unit
    member StartGameWith : rpgClass:RpgClass -> unit
  end
val canvas : Canvas
val menu : StartMenu

