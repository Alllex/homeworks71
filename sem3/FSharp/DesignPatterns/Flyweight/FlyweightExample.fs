module FlyweightExample

open System.Drawing
open System.Collections.Generic

type IShape =
    abstract member Print : Point[] -> unit

type Polygon(n : int, printingFunction) = 
    
    interface IShape with
        member this.Print(points) =
            printingFunction points

type ShapeFactory(printingFunction) =

    let shapes = new Dictionary<int, IShape>()
    let mutable totalCreated = 0
    let mutable totalRequested = 0

    member this.GetShape(n : int) =
        totalRequested <- 1 + totalRequested
        if shapes.ContainsKey(n) 
        then 
            shapes.[n]
        else 
            totalCreated <- 1 + totalCreated
            let newShape = new Polygon(n, printingFunction)
            shapes.Add(n, newShape)
            newShape :> IShape

    member this.TotalCreated = totalCreated
    member this.TotalRequested = totalRequested