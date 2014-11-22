
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
        
    override this.Equals obj = 
        match obj with 
        | :? WeightedVertex as other -> this.ID = other.ID
        | _ -> false
     
    override this.GetHashCode() = this.ID ^^^ 31
        
    interface IComparable<WeightedVertex> with
        member this.CompareTo (other : WeightedVertex) =
            this.ID.CompareTo other.ID
            
    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | :? WeightedVertex as other -> (this :> IComparable<_>).CompareTo other
            | _ -> invalidArg "obj" "not a WeightedVertext"
            
    
end

type IWeightedEdge = IEdge<WeightedVertex>

type WeightedEdge(source : WeightedVertex, target : WeightedVertex, weight : int) = class
    new(e : IEdge<WeightedVertex>, weight : int) = 
        WeightedEdge(e.Source, e.Target, weight)
    member this.Weight = weight
    interface IEdge<WeightedVertex> with
        member this.Source = source
        member this.Target = target
        
    member this.Source = (this :> IWeightedEdge).Source
    member this.Target = (this :> IWeightedEdge).Target 
           
    override this.ToString() = 
        let ie = this :> IEdge<WeightedVertex>
        ie.Source.ToString() + " --(" + weight.ToString() + ")-- " + ie.Target.ToString()
        
    override this.Equals obj = 
        match obj with 
        | :? WeightedEdge as other -> this.Source.Equals(other.Source) && this.Target.Equals(other.Target)
        | _ -> false
     
    override this.GetHashCode() = this.Source.GetHashCode() &&& this.Target.GetHashCode()
                             
    interface IComparable<WeightedEdge> with
        member this.CompareTo (other : WeightedEdge) =
            let this' = this :> IWeightedEdge
            let other' = other :> IWeightedEdge
            if (this'.Source = other'.Source && this'.Target = other'.Target ||
                this'.Target = other'.Source && this'.Source = other'.Target)
            then 0 else 1
            
    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | :? WeightedEdge as other -> (this :> IComparable<_>).CompareTo other
            | _ -> invalidArg "obj" "not a WeightedEdge"
            
end
 
type Graph = UndirectedGraph<LabeledVertex, Edge>
type WeightedGraph = UndirectedGraph<WeightedVertex, WeightedEdge>

type SpanVerticies = Set<WeightedVertex>
type SpanEdges = Set<WeightedEdge>
type GraphSeqElem = E of WeightedEdge
type GraphSeq = GraphSeqElem list






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

let lightestEdges (qw : WeightedGraph) =
    if Seq.length qw.Edges = 1 then qw.Edges
    else
        let lightestEdge = Seq.minBy (fun (e: WeightedEdge) -> e.Weight) qw.Edges
        qw.Edges
        |> Seq.filter (fun (e : WeightedEdge) -> e.Weight = lightestEdge.Weight)
       

let selectFirstEdge (p : WeightedEdge seq) (qw : WeightedGraph) =
    if (Seq.length p > 1) then
        
        let degree (v : WeightedVertex) =
            Seq.length <| qw.AdjacentEdges v
        let sumDegree (e : WeightedEdge) =
            let e' = e :> IWeightedEdge
            degree e'.Source + degree e'.Target
            
        Seq.minBy sumDegree qw.Edges
        // commented code - in case of need randomly choosing from satisfied elements
        (*
        let minSumDegEdge = 
            Seq.reduce (fun (curr : WeightedEdge) (next : WeightedEdge) -> 
                            if (sumDegree next < sumDegree curr) then next else curr
                       )
            <| qw.Edges
        let minSum = sumDegree minSumDegEdge
        let p' = qw.Edges |> Seq.filter (fun (e : WeightedEdge) -> sumDegree e = minSum)
        Seq.head p'
        *)
    else
       Seq.head p
       

let indGCount (qw : WeightedGraph) (vs : SpanVerticies) =
    let indG = qw.Edges |> Seq.filter (fun e -> Set.contains e.Source vs && Set.contains e.Target vs)
    Seq.length indG


let selectSpanningEdge (p : WeightedEdge seq) (qw : WeightedGraph) (vt : SpanVerticies) =
    let p1 = 
        if Seq.length p = 1 then p
        else
            let ind (e : WeightedEdge) = indGCount qw (Set.add e.Target vt)
            let sample = qw.Edges |> Seq.maxBy ind
            let sampleValue = ind sample
            qw.Edges
            |> Seq.filter (fun e -> sampleValue = ind e)
    let p2 = 
        if Seq.length p1 = 1 then p1
        else
            let degree (e : WeightedEdge) =
                Seq.length <| qw.AdjacentEdges e.Target
            let sample = qw.Edges |> Seq.minBy degree
            let sampleValue = degree sample
            qw.Edges
            |> Seq.filter (fun e -> sampleValue = degree e)
    Seq.head p2

let front (qw : WeightedGraph) (vt : SpanVerticies) =
    qw.Edges
    |> Seq.filter (fun e ->
                      Set.contains e.Source vt && not (Set.contains e.Target vt) // doesn't process undirectiness
                  )
                  
let inner (qw : WeightedGraph) (vt : SpanVerticies) =
    qw.Edges
    |> Seq.filter (fun e ->
                      Set.contains e.Source vt && Set.contains e.Target vt
                  )

let minimumSpanningTree (qw : WeightedGraph) = 
    let vs = Set.ofSeq qw.Vertices
    let mutable vt = Set.empty<WeightedVertex>
    let mutable et = Set.empty<WeightedEdge>
    let mutable seq = []
    let mutable p = lightestEdges qw
    let mutable e = selectFirstEdge p qw
    et <- Set.add e et
    vt <- Set.add e.Source vt
    vt <- Set.add e.Target vt
    while Set.difference vt vs <> Set.empty do
        p <- front qw vt
        e <- selectSpanningEdge p qw vt
        et <- Set.add e et
        vt <- Set.add e.Target vt // doesn't check undirected
        seq <- E e :: seq
        qw.RemoveEdge e |> ignore
        let inn = inner qw vt |> Seq.sortBy (fun e -> e.Weight)
        for ee in inn do
            seq <- E ee :: seq
            qw.RemoveEdge ee |> ignore
    (et , seq)
    
// ---------------------------------------------------------------------------------------------------------


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

