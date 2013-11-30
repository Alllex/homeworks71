module FlyweightGeneral

open System.Collections.Generic

type Flyweight =
    abstract member StatefulOperation : obj -> unit

type ConcreteFlyweight() = 

    interface Flyweight with
        member this.StatefulOperation(o) = () // do something useful

type UnsharedFlyweight() = 

    let mutable state = None

    interface Flyweight with
        member this.StatefulOperation(o) = 
            state <- Some o
            () // do something useful

type FlyweightFactory() =

    let flyweights = new Dictionary<_,_>()

    member this.GetFlyweight(key) =
        if flyweights.ContainsKey(key) 
        then 
            flyweights.[key] :> Flyweight
        else 
            let newFlyweight = new ConcreteFlyweight()
            // configure new flyweight here
            flyweights.Add(key, newFlyweight)
            newFlyweight :> Flyweight