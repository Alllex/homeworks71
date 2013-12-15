module MatrixMultiplicator

open System

let rand = new Random()

type Matrix(n : int) = 

    let getRandMatrix n =
        let getRandArray n =
            let max = 9
            [| for i in 0..(n - 1) -> rand.Next(max) + 1 |]
        [| for i in 0..(n - 1) -> getRandArray n |]

    let matrix = getRandMatrix n

    let createThreadTask (arr : int[,]) (m2 : int[][]) taskCount threadIndex =
        let start = threadIndex * taskCount
        async {
            for i in start..(start + taskCount - 1) do
                for j in 0..(n - 1) do
                    Array2D.set arr i j (List.fold (fun acc k -> acc + matrix.[i].[k] * m2.[k].[j]) 0 [0..(n - 1)])
        }

    member this.Matrix = matrix

    member this.Side = n

    member this.Mult(m : Matrix, countThreads : int) =
        let arr = Array2D.zeroCreate n n
        let taskCount = n / countThreads
        let task = createThreadTask arr m.Matrix taskCount
        [0..(countThreads - 1)]
        |> Seq.map task
        |> Async.Parallel 
        |> Async.RunSynchronously
        |> ignore
        arr

    member this.Print =
        for i in 0..(n - 1) do
            matrix.[i] |> printfn "%A"