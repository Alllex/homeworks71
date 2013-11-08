module Magic

let internal rand = new System.Random()

type DaylightType = Morning | Noon | Afternoon | Night

type CreatureType = Puppy
                  | Kitten
                  | Hedgehog
                  | Bearcub
                  | Piglet
                  | Bat
                  | Ballon

type IDaylight = 
    abstract member Current : DaylightType

type ILuminary = 
    abstract member IsShining : unit -> bool

type IWind =
    abstract member Speed : int

type IMagic =
    abstract member CallCourier : CreatureType -> unit

type Daylight() = 
    interface IDaylight with
        member x.Current = 
            let r = rand.Next(100)
            if r > 75 then DaylightType.Morning
            else if r > 50 then DaylightType.Noon
                 else if r > 25 then DaylightType.Afternoon
                      else DaylightType.Night

type Luminary() = 
    interface ILuminary with
        member x.IsShining() = rand.Next(100) > 50

type Wind() =
    interface IWind with
        member x.Speed = rand.Next(11)

type Creature(ct : CreatureType) =
    member x.CreatureType = ct

type Magic() = 
    interface IMagic with
        member x.CallCourier(ct : CreatureType) = ()

type IWeatherFactory = 
    abstract member CreateDaylight : unit -> IDaylight
    abstract member CreateLuminary : unit -> ILuminary
    abstract member CreateWind : unit -> IWind

type WeatherFactory() =
    interface IWeatherFactory with
        member x.CreateDaylight() = new Daylight() :> IDaylight
        member x.CreateLuminary() = new Luminary() :> ILuminary
        member x.CreateWind() = new Wind() :> IWind



type Cloud(factory : IWeatherFactory) =
    let daylight = factory.CreateDaylight()
    let luminary = factory.CreateLuminary()
    let wind = factory.CreateWind()
 
    member private x.InternalCreate() =
        let ct = match (daylight.Current, luminary.IsShining(), wind.Speed) with
                 | (Afternoon, true, s) when (s >= 0 && s <= 3) -> CreatureType.Puppy
                 | (Morning, true, 0) -> CreatureType.Kitten
                 | (Morning, false, s) when (s >= 3 && s <= 8) -> CreatureType.Piglet
                 | (Night, true, s) when (s >= 4 && s <= 6) -> CreatureType.Bat
                 | (Noon, false, s) when (s >= 3 && s <= 7) -> CreatureType.Hedgehog
                 | (Noon, false, s) when (s >= 6 && s <= 10) -> CreatureType.Bearcub
                 | _ -> CreatureType.Ballon

        new Creature(ct)
 
    member x.Create() =
      let creature = x.InternalCreate()
      let magic = new Magic() :> IMagic 
      magic.CallCourier(creature.CreatureType)
      creature


