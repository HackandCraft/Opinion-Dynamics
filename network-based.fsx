module NetworkBased
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


let rec combinations acc size set = seq {
  match size, set with 
  | n, x::xs -> 
      if n > 0 then yield! combinations (x::acc) (n - 1) xs
      if n >= 0 then yield! combinations acc n xs 
  | 0, [] -> yield acc 
  | _, [] -> () }


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
// Operations for working with beliefs
// --------------------------------------------------------------------------------------
module Beliefs = 
  let private domainIndexLookup =
    seq { for i, dom in Seq.indexed ideas do   
            for belief in dom do  
              yield belief, (i, dom) } |> dict

  /// Returns index of a domain into which the given belief belongs
  let getDomainIndex b = 
    domainIndexLookup.[b] |> fst

  /// Returns a domain of a belief (i.e., other conflicting beliefs)
  let getDomain b = 
    domainIndexLookup.[b] |> snd

  /// Are the two beliefs conflicting?
  let conflict a b = 
    getDomainIndex a = getDomainIndex b

  let private colors = [|
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

  /// Returns a HTML color of a given belief 
  /// TODO: This only works for domains with <= 4 beliefs
  let colorBelief = 
    let lookup = 
      seq { for shades, domain in Seq.zip colors ideas do
              for color, belief in Seq.zip shades domain do 
                yield belief, color } |> dict
    fun b -> lookup.[b]


// --------------------------------------------------------------------------------------
// Operations for working with a graph
// --------------------------------------------------------------------------------------

module Graph = 
  let getEdges id g = 
    g.Edges |> Seq.filter (fun e -> e.Nodes.Contains(id)) 

  let getNeighbours a g = 
    g.Edges |> Seq.choose (fun e ->
      if e.Nodes.Contains(a.ID) then Some { ID = e.Nodes.Other(a.ID) }
      else None )

  let updateBelief edge newBelief g = 
    { g with Edges = g.Edges |> Array.map (fun old ->
          if old = edge then { edge with Belief = newBelief } else old) }

  let existsEdge a1 a2 g = 
    g.Edges |> Seq.exists (fun e -> e.Nodes = unpair a1 a2)

  let removeEdge edge g = 
    { g with Edges = g.Edges |> Array.filter (fun old -> old <> edge) }

  let addEdge edge g = 
    { g with Edges = Array.append [|edge|] g.Edges }

  let addEdges edges g = 
    { g with Edges = Array.append edges g.Edges }

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


module Stats = 

  let averageDegree (g:Graph) = 
    g.Agents 
    |> Seq.map (fun a -> 
      let neighbours = Graph.getEdges a.ID g 
      float (Seq.length neighbours) ) 
    |> Seq.average
  
  let clusteringCoeffcient (g:Graph) =
    g.Agents 
      |> Seq.map (fun a -> Graph.getNeighbours a g)
      |> Seq.map (fun s -> s |> Seq.toList)
      |> Seq.map (fun nl -> combinations [] 2 nl) //get all possible triads
      |> Seq.concat
      |> Seq.map (fun  x -> (x |> Seq.head, x |> Seq.last ))
      |> Seq.map (fun (f, s) -> if (Graph.existsEdge (f.ID) (s.ID) g) = true then 1.0 else 0.0)
      |> (fun x -> (x |> Seq.sum) / float (x |> Seq.length)) // ratio between actual triads and possible triads
 
  
  let rec neighboursContainTarget (g: Graph) (target: Agent) (length: int)  (neighbours: Agent seq)  =
    neighbours
    |> Seq.exists (fun a -> a = target) 
    |> (fun tf -> match tf with 
                  | true -> length
                  | false -> neighbours
                              |> Seq.map (fun n -> Graph.getNeighbours n g)
                              |> Seq.concat
                              |> neighboursContainTarget g target (length + 1)
      )
  
  let shortestPath (a1: Agent) (a2: Agent) (g: Graph)  = 
    neighboursContainTarget  (g: Graph)  a2 1 (Graph.getNeighbours a1 g)
   
    

// --------------------------------------------------------------------------------------
// Visualizing network
// --------------------------------------------------------------------------------------

module Vis = 
  let filterGraph domain (g:Graph) =
    { g with Edges = g.Edges |> Array.filter (fun e -> Set.contains e.Belief domain) }

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
// Basic logic operations for composing graph transforamtions
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

  let applyOneProb ops g = 
    let sum = Seq.sumBy fst ops
    let r = rnd.NextDouble() * sum
    let mutable cummulative = 0.0
    let _, op = ops |> Seq.find (fun (p, op) -> 
      cummulative <- cummulative + p
      r <= cummulative)
    op g

// --------------------------------------------------------------------------------------
// Operations of the simulation
// --------------------------------------------------------------------------------------

module Sim = 
  let private rnd = System.Random() 
  let log = Event<string>()

  // HELPERS

  let getAgentBeliefs a g = 
    [| for e in Graph.getEdges a g -> e.Belief |]

  // ACTIONS
  // All of these take some (randomly picked) options 
  // and modify graph according to those options

  let addEdge (first, second, belief) g = 
    log.Trigger $"Adding {first}<--({belief})-->{second}"
    Graph.addEdge { Nodes = unpair first second; Belief = belief } g

  let adoptBelief (agent, belief, neighbours) g = 
    let edges = 
      [| for n in neighbours -> 
           log.Trigger $"Adding {agent.ID}<--({belief})-->{n.ID}"
           { Nodes = unpair agent.ID n.ID; Belief = belief } |]
    Graph.addEdges edges g

  let updateBeliefOnEdge edge g = 
    let domain = Beliefs.getDomain edge.Belief
    let firstBeliefs = getAgentBeliefs edge.Nodes.First g 
    let secondBeliefs = getAgentBeliefs edge.Nodes.Second g
    // TODO: 'edge' is currently included twice - do we want that?
    let neighbourBeliefs = 
      Seq.append firstBeliefs secondBeliefs
      |> Seq.filter domain.Contains
      |> Array.ofSeq
    let newBelief = neighbourBeliefs.[rnd.Next neighbourBeliefs.Length]
    log.Trigger $"Adopting {newBelief} on {edge.Nodes.First}<--({edge.Belief})-->{edge.Nodes.Second}"
    Graph.updateBelief edge newBelief g
  
  let removeEdge edge g = 
    log.Trigger $"Removing {edge.Nodes.First}<--({edge.Belief})-->{edge.Nodes.Second}"
    Graph.removeEdge edge g

  // OPTION GENERATORS
  // All of these take a graph and generate an array of options for one of the above actions 

  let getEdges g = g.Edges

  let getConnectedAgents g = 
    let conn = g.Edges |> Seq.collect (fun e -> [e.Nodes.First; e.Nodes.Second]) |> set
    g.Agents |> Array.filter (fun a -> conn.Contains a.ID)

  let getDisconnectedAgents g = 
    let conn = g.Edges |> Seq.collect (fun e -> [e.Nodes.First; e.Nodes.Second]) |> set
    g.Agents |> Array.filter (fun a -> not (conn.Contains a.ID))

  let private getBeliefsToAdopt a g = 
    // Get beliefs from domains that this agent has some opinion about
    let myDomains = getAgentBeliefs a.ID g |> Seq.map Beliefs.getDomain |> Seq.fold Set.union Set.empty
    let neighbours = Graph.getNeighbours a g 
    let potentialLinks = 
      [ for n in neighbours do
          let otherBeliefs = getAgentBeliefs n.ID g |> Seq.filter (fun b -> not (myDomains.Contains b))
          for ob in Seq.distinct otherBeliefs -> ob, n ]      
    potentialLinks 
    |> Seq.groupBy fst
    |> Seq.choose (fun (b, agents) -> 
        if Seq.length agents < 2 then None
        else Some(a, b, Seq.map snd agents))

  let getAgentsWithBeliefsToAdopt g = 
    g.Agents |> Seq.collect (fun a -> getBeliefsToAdopt a g) |> Array.ofSeq  

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
  
  // MAIN
  // Iteratively transform the graph using a specified transformation 'f'

  let rec iterate n f g = seq { 
    if n > 0 then 
      yield g
      let g = f g
      yield! iterate (n - 1) f g
    else 
      yield g }

  let iterateWithLog n f g = 
    let msgs = ResizeArray<string>()
    use _ = log.Publish.Subscribe(msgs.Add)
    let formatLog() = $"""<ul>{String.concat "" [for m in msgs -> $"<li>{m}</li>"]}</ul>"""
    iterate n f g |> Array.ofSeq, formatLog()

// FOR USE IN F# INTERACTIVE

let experiments () = 
  let g = Graph.initGraph 5 10  
  Graph.printGraph g
  
  let update = 
    Logic.applyOne [
      // Adopt new belief
      Logic.withOne Sim.getAgentsWithBeliefsToAdopt
        Sim.adoptBelief
      // Connect two disconnected agents with shared belief
      Logic.withOne Sim.getDisconnectedCompatibleAgents
        Sim.addEdge
      // Switch an edge (to a new belief based on neighbour's beliefs)
      Logic.withOne Sim.getEdges 
        Sim.updateBeliefOnEdge
      // "tired of all the arguments" Stop believing in conflicting thing
      Logic.withOne Sim.getConflictingEdges 
        Sim.removeEdge

      Logic.withOne Sim.getDisconnectedAgents (fun a1 ->
        Logic.withOne Sim.getConnectedAgents (fun a2 ->
          Logic.withOne (Sim.getAgentBeliefs a2.ID) (fun belief ->
            Sim.addEdge (a1.ID, a2.ID, belief))))
    ]
  ()