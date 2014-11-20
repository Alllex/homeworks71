
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

type WeightedVertex(label : String, weight : int) = class
    inherit LabeledVertex(label)
    new(v : LabeledVertex, weight : int) = WeightedVertex(v.Label, weight)
    member this.Weight = weight
    override this.ToString() =
        this.ID.ToString() + "[" + label + "](" + weight.ToString() + ")"
end

type WeightedEdge(source : WeightedVertex, target : WeightedVertex, weight : int) = class
    new(e : IEdge<WeightedVertex>, weight : int) = 
        WeightedEdge(e.Source, e.Target, weight)
    member this.Weight = weight
    interface IEdge<WeightedVertex> with
        member this.Source = source
        member this.Target = target
        
    override this.ToString() = 
        let ie = this :> IEdge<WeightedVertex>
        ie.Source.ToString() + " --(" + weight.ToString() + ")-- " + ie.Target.ToString()
end
 
type Graph = UndirectedGraph<LabeledVertex, Edge>
type WeightedGraph = UndirectedGraph<WeightedVertex, WeightedEdge>

let vertexFreqTable (q : Graph) (g : Graph) =
    let gVertices = List.ofSeq g.Vertices
    let countEntries lbl = List.length <| List.filter (fun (v : LabeledVertex) -> v.Label = lbl) gVertices
    let lbls = q.Vertices |> Seq.map (fun v -> v.Label) |> Seq.distinct |> Seq.toList 
    new Map<String, int>(List.map (fun lbl -> (lbl, countEntries lbl)) lbls)
    
    
let edgeFreqTable (q : Graph) (g : Graph) =
    let edge2lbls = (fun (e : Edge) -> 
                        let e' = e :> IEdge<LabeledVertex>
                        (e'.Source.Label, e'.Target.Label)
                    )
    let gEdgeLabels = 
         List.ofSeq g.Edges 
         |> List.map edge2lbls
    let countEntries srcl tgtl =
         List.length
         <| List.filter (fun (l1, l2) -> l1 = srcl && l2 = tgtl) gEdgeLabels
    let edgeLabels = 
        q.Edges
        |> Seq.map edge2lbls
        |> Seq.distinct
        |> Seq.toList
    List.map (fun (l1, l2) -> ((l1, l2), countEntries l1 l2 + countEntries l2 l1)) edgeLabels
    |> fun lst -> new Map<String * String, int>(lst)
    
let mkWeightedGraph (q : Graph) (g : Graph) = 
    let lblWeights = vertexFreqTable q g
    let edgeWeights = edgeFreqTable q g
    let q' = new WeightedGraph(false)
    q'.AddVerticesAndEdgeRange
    <| Seq.map (fun (e : Edge) -> 
                    let e' = e :> IEdge<LabeledVertex>
                    let src = e'.Source.Label
                    let tgt = e'.Target.Label
                    let sourceWeight = lblWeights.Item src
                    let targetWeight = lblWeights.Item tgt
                    let edgeWeight = edgeWeights.Item (src, tgt)
                    let vSrc = new WeightedVertex(src, sourceWeight)
                    let vTgt = new WeightedVertex(tgt, targetWeight)
                    new WeightedEdge(vSrc, vTgt, edgeWeight)
                ) q.Edges
    |> ignore
    q'

let print (s : String) = Console.WriteLine(s)

let printWeightedGraph (g : WeightedGraph) =
    for e in g.Edges do
        print (e.ToString())

let mkGraph (vs : String list) (es : (int * int) list) = 
    let g = new Graph(false)
    let verticies = List.map (fun lbl -> new LabeledVertex(lbl)) vs
    let edges = List.map (fun (src, tgt) -> new Edge(verticies.Item src, verticies.Item tgt)) es
    g.AddVertexRange verticies |> ignore
    g.AddEdgeRange edges |> ignore
    g
    

[<EntryPoint>]
let main args = 
    print "Hi"
    
    let q = mkGraph ["A"; "B"; "B"; "C"] [(0, 1); (0, 2); (0, 3)]
    let g = mkGraph ["C"; "A"; "A"; "A"; "A"; "B"; "B"; "B"; "B"] [(0, 1); (0, 2); (0, 3); (0, 4); (1, 5); (1, 6); (2, 5); (2, 7); (3, 6); (3, 8); (4, 7); (4, 8)]
    
    let q' = mkWeightedGraph q g
    
    printWeightedGraph q'
    
    0

