(*
    Program: OOP Pokemon World
    Author: Alex Semin Math-Mech 271 2013
    2013 (c)
*)


#if INTERACTIVE
#time "on"
#endif

type PokemonWorldObj() = inherit obj()

[<AbstractClass>]
type Pokemon(name : string, hp : int, attack : int, defence : int) = 
  class
    inherit  PokemonWorldObj()
    let name = name
    let mutable hp = hp
    let attack = attack
    let defence = defence
    let mutable xp = 0
    member this.Name = name
    member this.Info = sprintf "Name: \"%s\" [ HP(%d), AP(%d), DP(%d) ]" name hp attack defence
    abstract member Hit : unit -> string
    abstract member Defend : unit -> string
  end 

type Pickachu() =
    inherit Pokemon("Pickachu",100,10,7)
    override this.Hit() = "Lightning!"
    override this.Defend() = "Electricity Shield!!"

type Meowth() =
    inherit Pokemon("Meowth",60,7,10)
    override this.Hit() = "Clutch!"
    override this.Defend() = "Run!!!"

type Slowpoke() =
    inherit Pokemon("Slowpoke",200,5,20)
    override this.Hit() = "Later!"
    override this.Defend() = "Z-z-z-z..."

type PokeBall() =
  class
    inherit PokemonWorldObj()
    let mutable pokemon = None

    member this.HasPokemon = Option.isNone pokemon

    member this.ReleasePokemon() = 
        let p = pokemon
        do pokemon <- None
        p

    member this.CatchPokemon(p : Pokemon) = 
        do pokemon <- Some(p)
        this

    override this.ToString() = 
        match pokemon with
        | Some p -> p.Info
        | None -> "Empty PokeBall"

    static member ToString(pokeball : PokeBall) = pokeball.ToString() 
  end

type Person(name : string) =
    class
        inherit PokemonWorldObj()
        let name = name
        let pokemons = new ResizeArray<PokeBall>()
        member this.TakePokemon(p : Pokemon) = pokemons.Add((new PokeBall()).CatchPokemon(p))
        member this.ShowPokemons() = 
            printfn "%A" (name + "'s pokemons:")
            pokemons.ForEach (new System.Action<PokeBall>(fun (pb : PokeBall) -> pb.ToString() |> printfn "%s"))
    end


let Ash = new Person("Ash")
Ash.TakePokemon(new Pickachu())
Ash.TakePokemon(new Slowpoke())
let Jessy = new Person("Jessy R")
Jessy.TakePokemon(new Meowth())

Ash.ShowPokemons()
Jessy.ShowPokemons()

printfn "%A" <| (new Meowth()).Hit()
printfn "%A" <| (new Slowpoke()).Defend()