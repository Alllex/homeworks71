module Tests

open Magic
open FsUnit
open NUnit.Framework

type DaylightFake(dt : DaylightType) = 
    interface IDaylight with
        member x.Current = dt

type LuminaryFake(lc : bool) = 
    interface ILuminary with
        member x.IsShining() = lc

type WindFake(ws : int) =
    interface IWind with
        member x.Speed = ws

type WeatherFactoryFake(dt : DaylightType, lc : bool, ws : int) =
    interface IWeatherFactory with
        member x.CreateDaylight() = DaylightFake(dt) :> IDaylight
        member x.CreateLuminary() = LuminaryFake(lc) :> ILuminary
        member x.CreateWind() = new WindFake(ws) :> IWind

[<TestFixture>]
type ``Cloud tests`` () =
    
    [<Test>]
    member x.``Create puppy`` () =
       (new Cloud(new WeatherFactoryFake(DaylightType.Afternoon, true, 2) :> IWeatherFactory)).Create().CreatureType 
        |> should equal CreatureType.Puppy

    [<Test>]
    member x.``Create kitten`` () =
       (new Cloud(new WeatherFactoryFake(DaylightType.Morning, true, 0) :> IWeatherFactory)).Create().CreatureType 
        |> should equal CreatureType.Kitten

    [<Test>]
    member x.``Create piglet`` () =
       (new Cloud(new WeatherFactoryFake(DaylightType.Morning, false, 5) :> IWeatherFactory)).Create().CreatureType 
        |> should equal CreatureType.Piglet

    [<Test>]
    member x.``Create bat`` () =
       (new Cloud(new WeatherFactoryFake(DaylightType.Night, true, 5) :> IWeatherFactory)).Create().CreatureType 
        |> should equal CreatureType.Bat

    [<Test>]
    member x.``Create hedgehog`` () =
       (new Cloud(new WeatherFactoryFake(DaylightType.Noon, false, 7) :> IWeatherFactory)).Create().CreatureType
        |> should equal CreatureType.Hedgehog

    [<Test>]
    member x.``Create bearcub`` () =
       (new Cloud(new WeatherFactoryFake(DaylightType.Noon, false, 10) :> IWeatherFactory)).Create().CreatureType
        |> should equal CreatureType.Bearcub

    [<Test>]
    member x.``Create ballon`` () =
       (new Cloud(new WeatherFactoryFake(DaylightType.Night, true, 10) :> IWeatherFactory)).Create().CreatureType
        |> should equal CreatureType.Ballon