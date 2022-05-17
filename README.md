# Opinion Dynamics modelling

* `agent-based.fsx` - Initial attempt where we model agents with beliefs and links 
  between them, also annotated with beliefs (i.e., reason for the link). This was
  abandoned as too complex.

* `network-based.fsx` - A new attampt where we have just a network and what agents 
  "believe" is represented just by the (labelled) links to other agents. So, what
  agent believes is just what their network believes.

* `analysis.ipynb` - Loads `network-based.fsx` and does various visualizations
  of both the network (using vis.js) and of various statistics over the network.

## Simulation

The most elaborate simulation that is used in `analysis.ipynb` implements the following
rules:

```
let update = 
  Logic.applyOneProb [
    // Adopt belief in a new domain based on beliefs in other domain
    0.1, Logic.withOne Sim.getAgentsWithBeliefsToAdopt
      Sim.adoptBelief
    // Connect two disconnected agents with shared belief
    0.1, Logic.withOne Sim.getDisconnectedCompatibleAgents
      Sim.addEdge
    // Switch a belief on an edge to a new belief based on neighbour beliefs
    0.3, Logic.withOne Sim.getEdges 
      Sim.updateBeliefOnEdge
    // Stop believing in conflicting thing ("tired of all the arguments")
    0.3, Logic.withOne Sim.getConflictingEdges 
      Sim.removeEdge
    // Randomly remove edge to keep the average number of edges
    0.1, Logic.withOne Sim.getEdges 
      Sim.removeEdge
    // Connect agent that has been completely disconnected
    0.1, Logic.withOne Sim.getDisconnectedAgents (fun a1 ->
      Logic.withOne Sim.getConnectedAgents (fun a2 ->
        Logic.withOne (Sim.getAgentBeliefs a2.ID) (fun belief ->
          Sim.addEdge (a1.ID, a2.ID, belief))))
  ]
```

## Statistics

After running the simulation, we currently look at two statistics:

* _Average degree_, that is the average number of neighbours of each node (agent)
* _Clustering coefficient_, that is the number of triads (full graphs of 3 nodes) divided by the number of potential triads 