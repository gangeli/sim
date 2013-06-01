Similarity Measures
===

An implementation of common WordNet and Distributional similarity measures.

To Build
--------
Set `SCALA_HOME` in the Makefile as appropriate,
Or, use the included `dist/sim-release.jar` (with dependencies bundled
-- work out licensing at your own risk!) or
`dist/sim.jar` (without dependencies bundled) files.


To Use
------
To load the WordNet similarity measures, call:

     Ontology.load(/path/to/ontology)

The default path is `aux/ontology.ser.gz`.
To load the Distributional similarity measures, call:
     
     DistSim.load(/path/to/ontology)

The default path is `aux/distsim.ser.gz`.
From here, both classes have a method `sim(x, y)` that take two phrases
(or words) and computes a similarity object. This similarity object
then has the appropriate similarity metrics defined.
Common information is cached (as are the similarity values themselves),
so don't be afraid to call the functions multiple times!

Furthermore, if you have the models in implicit scope, you can call
`Similarity(x, y)`, which will yield a `Similarity` object implementing all
the metrics. For example:

     import org.goobs.sim._
     implicit val wordnet = Ontology.load("aux/ontology.ser.gz")
     implicit val distsim = DistSim.load("aux/distsim.ser.gz")
     val sim = Similarity("cat", "dog")
     println(sim.lesk)
     println(sim.cos)
