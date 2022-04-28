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
    Beliefs : List<Belief * float> 
    Susceptibility : float
    Trust : float }

type Link =
  { First : AgentID 
    Second : AgentID 
    Reason : Belief }

type Graph =
  { Agents : Agent list 
    Links : Link list }
  member x.GetAgent(id) = 
    x.Agents |> Seq.find (fun a -> a.ID = id)

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
          if rnd.NextDouble() > 0.2 then
            yield i.[rnd.Next(i.Length)], rnd.NextDouble() ] }

let initGraph () = 
  let agents = 
    [ for i in 0 .. 4 -> initAgent() ]
  let links = 
    [ for i in 0 .. 9 ->
        let fst = agents.[rnd.Next(agents.Length)]
        let snd = agents.[rnd.Next(agents.Length)]
        let beliefs = fst.Beliefs @ snd.Beliefs
        let reason, _ = beliefs.[rnd.Next(beliefs.Length)]
        { First = fst.ID; Second = snd.ID; Reason = reason } ]
    |> List.filter (fun l -> l.First <> l.Second) 
    // TODO: Also avoid conflicting reasons
  { Agents = agents; Links = links }

let printGraph graph = 
  printfn "AGENTS"
  for a in graph.Agents do
    printf $" * {a.ID}: " 
    for b, r in a.Beliefs do printf $" {b} @ %0.2f{r} |"
    printfn ""
  printfn "LINKS"
  for l in graph.Links do
    printfn $" {l.First} --({l.Reason})--> {l.Second}"

let chooseBelief beliefs = 
  let beliefs = Array.ofSeq beliefs
  seq { 
    while true do
      let b, r = beliefs.[rnd.Next(beliefs.Length)]
      if rnd.NextDouble() > r then
        yield b, rnd.NextDouble() }
  |> Seq.head
 
let updateAgent g a = 
  printfn $"UPDATING AGENT {a.ID}"
  for b, r in a.Beliefs do
    let domain = ideas |> Seq.find (fun dom -> dom.Contains b)
    let linkedBeliefs = 
      [ for l in g.Links do
          if l.First = a.ID then yield! g.GetAgent(l.Second).Beliefs
          if l.Second = a.ID then yield! g.GetAgent(l.First).Beliefs ]
      |> List.filter (fun (b, _) -> domain.Contains b)
      |> List.distinctBy fst
    let nb, nr = chooseBelief ((b, r) :: linkedBeliefs)
    printf $" - Belief: {b}(%0.1f{r}), Network: "
    for lb, lbr in linkedBeliefs do printf $"{lb}(%0.1f{lbr}) "
    printfn $"\n   Adopting: {nb}(%0.1f{nr})"

let g = initGraph()
printGraph g

for a in g.Agents do
  updateAgent g a

