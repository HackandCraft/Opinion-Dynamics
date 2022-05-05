module AgentBased
#r "nuget: FSharp.Data"
open FSharp.Data

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------

// Represents a two-element set (or un-ordered-pair)
// This is useful for storing nodes of a non-directed edge
type UnPair<'T when 'T : comparison> = 
  { First : 'T; Second : 'T }
  static member Create(a, b) = 
    let a, b = if a > b then b, a else a, b
    { First = a; Second = b }
  member x.Contains(v) = x.First = v || x.Second = v
  member x.Other(v) = 
    if x.First = v then x.Second 
    elif x.Second = v then x.First
    else failwith "Wrong argument to Other"

let unpair a b = UnPair<_>.Create(a, b)

// --------------------------------------------------------------------------------------
// Domain model
// --------------------------------------------------------------------------------------

type Belief = string
type Domain = Set<Belief>
type Ideas = List<Domain>

let ideas : Ideas = 
  [ set ["A1"; "A2"; "A3"; "A4" ]
    set ["B1"; "B2"; "B3"; "B4" ]
    set ["C1"; "C2"; "C3"; "C4" ]]


type AgentID = int

type Agent = 
  { ID : AgentID }

type Edge =
  { Nodes : UnPair<AgentID>
    Belief : Belief }

type Graph =
  { Agents : Agent[]
    Edges : Edge[] }

// --------------------------------------------------------------------------------------
// 
// --------------------------------------------------------------------------------------

module Beliefs = 
  let getDomain b = 
    ideas |> Seq.find (fun d -> d.Contains b)

  let conflict a b = 
    ideas |> Seq.exists (fun domain -> domain.Contains a && domain.Contains b)

  let colors = [|
    [|"#3182bd";"#6baed6";"#9ecae1";"#c6dbef"|]
    [|"#e6550d";"#fd8d3c";"#fdae6b";"#fdd0a2"|]
    [|"#31a354";"#74c476";"#a1d99b";"#c7e9c0"|]
    [|"#756bb1";"#9e9ac8";"#bcbddc";"#dadaeb"|]
    [|"#636363";"#969696";"#bdbdbd";"#d9d9d9"|] |]
//    [|"#393b79";"#5254a3";"#6b6ecf";"#9c9ede"|]; 
//    [|"#637939";"#8ca252";"#b5cf6b";"#cedb9c"|]; 
//    [|"#8c6d31";"#bd9e39";"#e7ba52";"#e7cb94"|]; 
//    [|"#843c39";"#ad494a";"#d6616b";"#e7969c"|]; 
//    [|"#7b4173";"#a55194";"#ce6dbd";"#de9ed6"|] |]

  let colorBelief = 
    let lookup = 
      seq { for shades, domain in Seq.zip colors ideas do
              for color, belief in Seq.zip shades domain do 
                yield belief, color } |> dict
    fun b -> lookup.[b]


// --------------------------------------------------------------------------------------
// 
// --------------------------------------------------------------------------------------

module Graph = 
  let getEdges id g = 
    g.Edges |> Seq.filter (fun e -> e.Nodes.Contains(id)) 

  let updateBelief edge newBelief g = 
    { g with Edges = g.Edges |> Array.map (fun old ->
          if old = edge then { edge with Belief = newBelief } else old) }

  let existsEdge a1 a2 g = 
    g.Edges |> Seq.exists (fun e -> e.Nodes = unpair a1 a2)

  let removeEdge edge g = 
    { g with Edges = g.Edges |> Array.filter (fun old -> old <> edge) }

  let addEdge edge g = 
    { g with Edges = Array.append [|edge|] g.Edges }

  let mutable agentCounter = 0
  let rnd = System.Random() 

  let initAgent () = 
    agentCounter <- agentCounter + 1
    { ID = agentCounter }

  let initGraph numberOfAgents numberOfEdges =
    let allBeliefs = ideas |> Seq.collect id |> Array.ofSeq
    let agents = 
      Array.init numberOfAgents (fun _ -> initAgent())
    let links = 
      Array.init numberOfEdges (fun _ ->
        let fst, snd = 
          Seq.initInfinite (fun _ -> rnd.Next(agents.Length), rnd.Next(agents.Length)) 
          |> Seq.filter (fun (a, b) -> a <> b) |> Seq.head
        let belief = allBeliefs.[rnd.Next(allBeliefs.Length)]
        { Nodes = unpair agents.[fst].ID agents.[snd].ID; Belief = belief } )
      // TODO: Also avoid conflicting reasons
    { Agents = agents
      Edges = links }

  let printGraph graph = 
    printfn "LINKS"
    for l in graph.Edges do
      printfn $" {l.Nodes.First} <--({l.Belief})--> {l.Nodes.Second}"

// --------------------------------------------------------------------------------------
// 
// --------------------------------------------------------------------------------------

module Vis = 
  let visualizeNetwork (g:Graph) = 
    let nodes = 
      JsonValue.Array [|
        for a in g.Agents ->
          JsonValue.Record([|   
            "id", JsonValue.Number (decimal a.ID)
            "label", JsonValue.String (string a.ID)
            "color", JsonValue.String "#303030"
            "size", JsonValue.Number 7M
            "font", JsonValue.Record [| 
              "color", JsonValue.String "#d0d0d0"
              "strokeWidth", JsonValue.Number 0M |]
            "borderWidth", JsonValue.Number 2M |])
      |]
    let edges = 
      JsonValue.Array [|
        for e in g.Edges ->
          JsonValue.Record([|   
            "color", JsonValue.String (Beliefs.colorBelief e.Belief)
            "width", JsonValue.Number 3M
            "font", JsonValue.Record [| 
              "color", JsonValue.String (Beliefs.colorBelief e.Belief)
              "strokeColor", JsonValue.String "black"
              "strokeWidth", JsonValue.Number 4M |]
            "label", JsonValue.String e.Belief
            "from", JsonValue.Number (decimal e.Nodes.First)
            "to", JsonValue.Number (decimal e.Nodes.Second) |])
      |]
    let id = "network" + System.Guid.NewGuid().ToString("N")
    let html = $"""
      <script>
        var nodes = new vis.DataSet({nodes.ToString()});
        var edges = new vis.DataSet({edges.ToString()});
        var container = document.getElementById("{id}");
        var data = {{ nodes: nodes, edges: edges, }};
        var options = {{ layout: {{ randomSeed: 2 }} }};
        var network = new vis.Network(container, data, options);
      </script>
      <div id="{id}" style="display:inline-block;width:500px;height:300px"></div>"""
    html

// --------------------------------------------------------------------------------------
// 
// --------------------------------------------------------------------------------------

module Logic = 
  let rnd = System.Random() 

  let withOne op cont g =
    let (arr:_[]) = op g
    if arr.Length > 0 then 
      cont arr.[rnd.Next(arr.Length)] g
    else 
      g

  let applyOne ops g = 
    (Seq.item (rnd.Next(Seq.length ops)) ops) g

module Sim = 
  let rnd = System.Random() 
  let log = Event<string>()

  let getEdges g = g.Edges

  let addEdge (first, second, belief) g = 
    log.Trigger $"Adding {first}<--({belief})-->{second}"
    Graph.addEdge { Nodes = unpair first second; Belief = belief } g
    
  let getAgentBeliefs a g = 
    Graph.getEdges a g 
    |> Seq.map (fun e -> e.Belief)

  let getConflictingEdges g = 
    g.Edges |> Array.filter (fun edge -> 
      let domain = Beliefs.getDomain edge.Belief
      let firstBeliefs = getAgentBeliefs edge.Nodes.First g
      let secondBeliefs = getAgentBeliefs edge.Nodes.Second g
      Seq.exists (fun b -> b <> edge.Belief && domain.Contains b) firstBeliefs ||
      Seq.exists (fun b -> b <> edge.Belief && domain.Contains b) secondBeliefs)

  let getDisconnectedCompatibleAgents g = 
    [| for first in g.Agents do  
         for second in g.Agents do
           let first, second = first.ID, second.ID
           if first <> second && not (Graph.existsEdge first second g) then 
             let firstBeliefs = getAgentBeliefs first g |> set
             let secondBeliefs = getAgentBeliefs second g |> set
             for domain in ideas do 
               let shared = domain |> Seq.filter (fun b -> firstBeliefs.Contains b && secondBeliefs.Contains b)
               for belief in shared do yield first, second, belief |]
  
  let updateBeliefOnEdge edge g = 
    let domain = Beliefs.getDomain edge.Belief
    let firstBeliefs = getAgentBeliefs edge.Nodes.First g 
    let secondBeliefs = getAgentBeliefs edge.Nodes.Second g
    let neighbourBeliefs = 
      Seq.append (Seq.append firstBeliefs secondBeliefs) [edge.Belief]
      |> Seq.filter domain.Contains
      |> Array.ofSeq
    let newBelief = neighbourBeliefs.[rnd.Next neighbourBeliefs.Length]
    log.Trigger $"Adopting {newBelief} on {edge.Nodes.First}<--({edge.Belief})-->{edge.Nodes.Second}"
    Graph.updateBelief edge newBelief g
  
  let removeEdge edge g = 
    log.Trigger $"Removing {edge.Nodes.First}<--({edge.Belief})-->{edge.Nodes.Second}"
    Graph.removeEdge edge g

  let rec iterate n f g = seq { 
    if n > 0 then 
      yield g
      let g = f g
      yield! iterate (n - 1) f g
    else 
      yield g }

let experiments () = 
  let g = Graph.initGraph 5 10

  let update = 
    Logic.applyOne [
      Logic.withOne Sim.getDisconnectedCompatibleAgents
        Sim.addEdge
      Logic.withOne Sim.getEdges 
        Sim.updateBeliefOnEdge
      Logic.withOne Sim.getConflictingEdges 
        Sim.removeEdge
    ]
    //Logic.pickOneNonEmpty g.Edges 
    //|> Sim.removeEdge

  ()