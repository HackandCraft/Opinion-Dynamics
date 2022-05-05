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

let unpair a b = UnPair<'a>.Create(a, b)

// --------------------------------------------------------------------------------------
// Domain model
// --------------------------------------------------------------------------------------

type Belief = string
type Domain = Set<Belief>
type Ideas = List<Domain>

let ideas : Ideas = 
  [ set ["A1"; "A2"; "A3"; "A4" ]
    set ["B1"; "B2"; "B3"; "B4" ]
    set ["C1"; "C2"; "C3"; "C4" ] ]

let conflict a b = 
  ideas |> Seq.exists (fun domain -> domain.Contains a && domain.Contains b)

type AgentID = int

/// A set of beliefs. This is a set that cannot, at the same time, contain 
/// multiple beliefs from a single domain of ideas. This is checked at runtime.
type BeliefSet(beliefs:Set<Belief>) = 
  do 
    let mutable conflicting = set []
    for belief in beliefs do
      if conflicting.Contains belief then failwith "Conflicted belief set!"
      let domain = ideas |> Seq.find (fun d -> d.Contains belief)
      conflicting <- Set.union conflicting (domain - set [belief])
  member x.All = beliefs
  member x.Contains(b) = beliefs.Contains(b)
  member x.Remove(b) = BeliefSet(beliefs.Remove(b))
  member x.Adopt(b) = 
    let domain = ideas |> List.find (fun d -> d.Contains b)
    BeliefSet((beliefs- domain).Add(b))

/// Agent consisting of an ID and a set of (non-conflicting) beliefs
type Agent = 
  { ID : AgentID
    Beliefs : BeliefSet }
  member x.RemoveBelief(b) = 
    { x with Beliefs = x.Beliefs.Remove(b) }
  member x.AdoptBelief(b) = 
    { x with Beliefs = x.Beliefs.Adopt(b) }

/// A link between two agents, created because of a shared belief 'Reason' at the time
type Edge =
  { Nodes : UnPair<AgentID>
    Reason : Belief }

/// A graph consisting of agents and edges (links) between them
type Graph =
  { Agents : Map<AgentID, Agent>
    Edges : Edge list }
  member x.GetAgent(id) = x.Agents.[id]
  member x.UpdateAgent(a) = 
    { x with Agents = x.Agents.Add(a.ID, a) }
  member x.GetEdges(id) = 
    x.Edges |> List.filter (fun e -> e.Nodes.Contains(id)) 
  member x.RemoveEdge(e) = 
    { x with Edges = x.Edges |> List.filter ((<>) e) }
  member x.AddEdge(e) = 
    { x with Edges = e :: x.Edges }
  member x.GetAgents() = x.Agents.Values

// --------------------------------------------------------------------------------------
// Initializing & printing the world
// --------------------------------------------------------------------------------------

let domainSkipChance = 0.2
let numberOfAgents = 5
let maxNumberOfEdges = 10

let mutable agentCounter = 0
let rnd = System.Random() 

let initAgent () = 
  agentCounter <- agentCounter + 1
  { ID = agentCounter 
    Beliefs =       
      [ // Generate at least one belief from one selected domain
        let oneDomainIdx = rnd.Next(ideas.Length)
        for i, domain in Seq.indexed ideas do
          let domain = Array.ofSeq domain
          if i = oneDomainIdx || rnd.NextDouble() > domainSkipChance then 
            yield domain.[rnd.Next(domain.Length)] ] |> set |> BeliefSet } 

let initGraph () = 
  let agents = 
    [ for i in 1 .. numberOfAgents -> initAgent() ]
  let links = 
    [ for i in 1 .. maxNumberOfEdges do 
        let fst = agents.[rnd.Next(agents.Length)]
        let snd = agents.[rnd.Next(agents.Length)]
        let beliefs = Set.union fst.Beliefs.All snd.Beliefs.All |> Array.ofSeq
        if beliefs.Length > 0 && fst.ID <> snd.ID then
          let reason = beliefs.[rnd.Next(beliefs.Length)]
          yield { Nodes = unpair fst.ID snd.ID; Reason = reason } ] 
    // TODO: Also avoid conflicting reasons
  { Agents = Map.ofSeq [ for a in agents -> a.ID, a ]
    Edges = links }

let printGraph graph = 
  printfn "AGENTS"
  for (KeyValue(_, a)) in graph.Agents do
    printf $" * {a.ID}: " 
    for b in a.Beliefs.All do printf $"{b} "
    printfn ""
  printfn "LINKS"
  for l in graph.Edges do
    printfn $" {l.Nodes.First} --({l.Reason})--> {l.Nodes.Second}"


// --------------------------------------------------------------------------------------
// Helpers for writing agent logic
// --------------------------------------------------------------------------------------

/// Try to pick one random element from a list
let tryPickOne l = 
  let a = Array.ofSeq l
  if a.Length > 0 then Some(a.[rnd.Next(a.Length)]) else None

/// Choose one of the selected operations and call it 
/// (if this was empty list, just return 'g' but this is not likely)
let choose ops v g = 
  match tryPickOne ops with
  | Some op -> op v g
  | _ -> g

/// Choose any of the inputs generated by 'f' and call the operation
/// 'op' with this input as argument (graph 'g' is implicitly passed to all)
let withAny f op g = 
  let msg, res = f g
  match tryPickOne res with 
  | Some v -> op v g
  | _ -> 
      printfn $"No {msg}"
      g

// --------------------------------------------------------------------------------------
// Modelling operations of the agents
// --------------------------------------------------------------------------------------

/// Returns conflicting agents involving an agent 'a'
/// i.e., edges where either or both agents no longer believe in the 'Reason' 
let conflictingEdges a (g:Graph) = 
  $"conflicting edges for agent {a.ID}",
  [ for e in g.GetEdges(a.ID) do      
      let conflict = 
        not (g.GetAgent(e.Nodes.First).Beliefs.Contains(e.Reason) &&
          g.GetAgent(e.Nodes.Second).Beliefs.Contains(e.Reason))
      if conflict then yield e ]

// RESOLVE CONFLICT

let removeBelief agent conflict (g:Graph) = 
  printfn $"Removing belief {conflict.Reason} from {agent.ID}"
  g.UpdateAgent(agent.RemoveBelief(conflict.Reason))

let removeLink agent conflict (g:Graph) = 
  printfn "Removing link %d--(%s)--%d"
    conflict.Nodes.First conflict.Reason conflict.Nodes.Second
  g.RemoveEdge(conflict)

let resolveConflict agent =  
  withAny (conflictingEdges agent) <| choose [
    removeBelief agent
    removeLink agent
  ]


/// Returns other agents in the network that believe in 'belief'
/// and do not currently have conflicting link wiht 'agent'
let potentialNeighbours agent belief (g:Graph) = 
  let agentLinks = g.GetEdges(agent.ID)
  let linksWith other = 
    agentLinks |> Seq.filter (fun link -> 
      link.Nodes = unpair agent.ID other.ID)
  $"other agents believing {belief} without conflicting link",
  g.GetAgents() 
  |> Seq.filter (fun other -> 
    other.ID <> agent.ID && other.Beliefs.Contains belief)
  |> Seq.filter (fun other -> 
    linksWith other |> Seq.forall (fun l -> not (conflict l.Reason belief)))

let beliefs agent (g:Graph) =
  $"beliefs of agent {agent.ID}", agent.Beliefs.All

let availableAgents (g:Graph) = 
  "available agents", g.GetAgents()

let currentNeighbors agent (g:Graph) = 
  $"neighbors of agent {agent.ID}",
  seq { for e in g.GetEdges(agent.ID) -> g.GetAgent(e.Nodes.Other(agent.ID)) }

let nonConflictingBeliefs this other g = 
  let conflicting = 
    [ for b in this.Beliefs.All do
        for domain in ideas do 
          if domain.Contains b then yield! domain ] |> set
  $"""beliefs not conflicting with {String.concat "," this.Beliefs.All}""",
  other.Beliefs.All |> Seq.filter (fun b -> not (conflicting.Contains b))


// ADD LINK / BELIEF & UPDATE GRAPH

let addNewLink agent = 
  withAny (beliefs agent) <| fun belief -> 
    withAny (potentialNeighbours agent belief) <| fun neighbor g ->
      printfn "Adding link %d--(%s)--%d" agent.ID belief neighbor.ID
      g.AddEdge({ Nodes = unpair agent.ID neighbor.ID; Reason = belief })

let adoptNewBelief agent =
  withAny (currentNeighbors agent) <| fun other ->
    withAny (nonConflictingBeliefs agent other) <| fun belief g ->
      printfn $"Agent {agent.ID} is adopting belief {belief}"
      g.UpdateAgent(agent.AdoptBelief(belief))

let updateGraph =
  withAny availableAgents <| choose [
    resolveConflict
    addNewLink
    adoptNewBelief
  ]

// MAIN LOOP & DEBUGGING

let rec loop n g = 
  if n > 0 then 
    //printfn $"==================== #{n} ===================="
    let g = updateGraph g
    //printGraph g
    loop (n - 1) g
  else g

let g = initGraph()
let g2 = loop 100000 g
printGraph g2

updateGraph g2 |> ignore

let (KeyValue(_, a)) = g.Agents |> Seq.head

// Is it possible to have a belief not shared by any other agent?
