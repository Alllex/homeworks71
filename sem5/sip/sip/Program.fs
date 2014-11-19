
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module sip.Main

open System
open QuickGraph

type LabeledVertex(label : String) = class
    static let mutable counter = 0
    let id = counter
    do counter <- counter + 1

    member this.Label = label
    member this.ID = id
end

type Edge(source : LabeledVertex, target : LabeledVertex) = class
    interface IEdge<LabeledVertex> with
        member this.Source = source
        member this.Target = target
end

type WeightedVertex(label : String, weight : float) = class
    inherit LabeledVertex(label)
    new(v : LabeledVertex, weight : float) = WeightedVertex(v.Label, weight)
    member this.Weight = weight
end

type WeightedEdge(v1 : LabeledVertex, v2 : LabeledVertex, weight : float) = class
    inherit Edge(v1, v2)
    new(e : IEdge<LabeledVertex>, weight : float) = 
        WeightedEdge(e.Source, e.Target, weight)
    member this.Weight = weight
end
 
type Graph = UndirectedGraph<LabeledVertex, Edge>


let vertexFreqTable (q : Graph) (g : Graph) =
    let gVertices = List.ofSeq g.Vertices
    let countEntries lbl = List.length 
                           <| List.filter (fun (v : LabeledVertex) -> v.Label = lbl) gVertices
    let lbls = q.Vertices |> Seq.map (fun v -> v.Label) |> Seq.distinct |> Seq.toList 
    new Map<String, int>(List.map (fun lbl -> (lbl, countEntries lbl)) lbls)
    
let edgeFreqTable (q: Graph) (g : Graph) =
    let gEdgeLabels = 
         List.ofSeq g.Edges 
         |> List.map (fun (e : IEdge<LabeledVertex>) -> (e.Source.Label, e.Target.Label))
    let countEntries srcl tgtl =
         List.length
         <| List.filter (fun (l1, l2) -> l1 = srcl && l2 = tgtl) gEdges
    let edgeLabels = 
         q.Edges
         |> Seq.map (fun e -> (e.Source.Label, e.Target.Label))
         |> Seq.distinct
         |> Seq.toList
    new Map<pair<String, String>, int>(List.map (fun (l1, l2) -> ((l1, l2), countEntries l1 l2 + countEntries l2 l1) edgeLabels)

let print (s : String) = Console.WriteLine(s)

[<EntryPoint>]
let main args = 
    print "Hi"
    let g = new Graph(false)
    let vs = List.map (fun lbl -> new LabeledVertex(lbl)) ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"]
    let es = List.map (fun (src, tgt) -> new Edge(vs.Item src, vs.Item tgt))
             <| [ (0, 1); (0, 3); (2, 5); (7, 5); (4, 2); (3, 6); (6, 1); (5, 1) ]
    g.AddVertexRange vs |> ignore
    g.AddEdgeRange es |> ignore
    
    0

