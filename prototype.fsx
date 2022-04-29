type Belief = string
type Domain = Set<Belief>
type Ideas = List<Domain>

let ideas : Ideas = 
  [ set ["A1"; "A2"; "A3"; "A4" ]
    set ["B1"; "B2"; "B3"; "B4" ]
    set ["C1"; "C2"; "C3"; "C4" ] ]



type AgentID = int

type Agent = 
  { ID : AgentID
    // TODO: No constraint there is one belief per domain
    Beliefs : List<Belief * float> 
    Susceptibility : float
    Trust : float }

type Edge =
  { First : AgentID 
    Second : AgentID 
    Reason : Belief 
    }

type Graph =
  { Agents : Agent list 
    Edges : Edge list }
  member x.GetAgent(id) = 
    x.Agents |> Seq.find (fun a -> a.ID = id)

// Comments
// - is the generated graph stable? 


let mutable agentCounter = 0
let rnd = System.Random() 

let initAgent () = 
  agentCounter <- agentCounter + 1
  { ID = agentCounter 
    Susceptibility = rnd.NextDouble()
    Trust = rnd.NextDouble()
    Beliefs = 
      [ for i in ideas do
          let i = Array.ofSeq i
          if rnd.NextDouble() > 0.2 then // TODO: Parameter
            yield i.[rnd.Next(i.Length)], rnd.NextDouble() ] }
      // TODO: But maybe we need at least 1 belief

// CONSIDER: Do we start in a more or less stable state?
// (if we start stable, does it destabilize? if we start unstable, does it stabilize?)
// (maybe config or maybe see what happens...)
let initGraph () = 
  let agents = 
    [ for i in 0 .. 4 -> initAgent() ]
  let links = 
    [ for i in 0 .. 9 -> // TODO: Parameter
        let fst = agents.[rnd.Next(agents.Length)]
        let snd = agents.[rnd.Next(agents.Length)]
        let beliefs = fst.Beliefs @ snd.Beliefs // TODO: intersection instead?
        let reason, _ = beliefs.[rnd.Next(beliefs.Length)]
        { First = fst.ID; Second = snd.ID; Reason = reason } ]
    |> List.filter (fun l -> l.First <> l.Second) 
    // TODO: Also avoid conflicting reasons
  { Agents = agents; Edges = links }

let printGraph graph = 
  printfn "AGENTS"
  for a in graph.Agents do
    printf $" * {a.ID}: " 
    for b, r in a.Beliefs do printf $" {b} @ %0.2f{r} |"
    printfn ""
  printfn "LINKS"
  for l in graph.Edges do
    printfn $" {l.First} --({l.Reason})--> {l.Second}"

let chooseBelief beliefs = 
  let beliefs = Array.ofSeq beliefs
  seq { 
    while true do
      let b, r = beliefs.[rnd.Next(beliefs.Length)]
      if rnd.NextDouble() > r then // TODO: What rate is right here?
        yield b, rnd.NextDouble() } // TODO: Random conviction is not right maybe
  |> Seq.head

// TODO: Instead of switching directly, we can decrease/increase until we switch
// We would have two different operations
// - updating conviction rate
// - dropping low beliefs; adding new beliefs

let updateAgent g a = 
  printfn $"UPDATING AGENT {a.ID}"
  // TODO: Cannot adopt belief about domain it is not thinking about already

  for b, r in a.Beliefs do
    let domain = ideas |> Seq.find (fun dom -> dom.Contains b)
    let linkedBeliefs = 
      [ for l in g.Edges do
          if l.First = a.ID then yield! g.GetAgent(l.Second).Beliefs
          if l.Second = a.ID then yield! g.GetAgent(l.First).Beliefs ]
      |> List.filter (fun (b, _) -> domain.Contains b)
      |> List.distinctBy fst // maybe pick the one with highest conviction? (or multiply)
    let nb, nr = chooseBelief ((b, r) :: linkedBeliefs)
    printf $" - Belief: {b}(%0.1f{r}), Network: "
    for lb, lbr in linkedBeliefs do printf $"{lb}(%0.1f{lbr}) "
    printfn $"\n   Adopting: {nb}(%0.1f{nr})"

let g = initGraph()
printGraph g

// NOTES
// You could have a distribution over domain instead of one belief
// (number for each belief in domain, adding up to 1) + extent to which you "care"

// MAIN LOOP
//   - (Switch belief)
//   - Adopt belief - for a domain you're not thinking about 
//   - Remove belief
//   - Add link
//   - Remove link

// [] Do  nothing 
// [add_belief] Adopt belief in new domain inherited from neighbour
// [add_link] Connect to new neighbor with same belief
// [remove_belief;add_belief] Switch  to neighbor belief within existing domain

// Conflicting belief => 
//   [remove_belief] Lapse
//   or [remove_link] Disconnect from a neighbour when either of you 



//  * Iterate over all agents:
//    - update beliefs of the agent (adopt neighbour's belief; remove my own belief)
//  * Update the links
//    - Remove some links (remove link with conflicting agent)
//    - Add new links (because of shared belief)

for a in g.Agents do
  updateAgent g a


// NETWORK CHARACTERISTICS (aka things to plot eventually)
// - What is the overall "Network instability" ? (conflicting links)
// - Belief popularity (number of links with that belief)
//
// How do we make sure domains do not end up perfectly overlapping?
