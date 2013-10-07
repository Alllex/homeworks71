
type PokemonWorldObj() = inherit obj()

[<AbstractClass>]
type Pokemon(name, hp, attack, defence) = 
  class
    inherit  PokemonWorldObj()
    let name : string = name
    let mutable hp : int = hp
    let attack : int = attack
    let defence : int = defence
    let mutable xp : int = 0
    member this.Name with get() = name
    member this.Info 
        with get() = "Name: \"" + name + 
                     "\" :: HP(" + hp.ToString() + 
                     "), AP(" + attack.ToString() + 
                     "), DP(" + defence.ToString() + ")"
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
    let mutable hasPokemon = false
    let mutable pokemon = None

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
  end

type Person(name) =
    class
        inherit PokemonWorldObj()
        let name : string = name
        let mutable pokemons = []
        member this.TakePokemon(p : Pokemon) = pokemons <- ((new PokeBall()).CatchPokemon(p))::pokemons
        member this.ShowPokemons() = 
            printfn "%A" (name + "'s pokemons:")
            pokemons |> List.iter (printfn "%A")
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