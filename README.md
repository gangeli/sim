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
the metrics. For example (from the project root):
      
     $ scala -Dwordnet.database.dir=aux/WordNet-3.1/dict -J-mx4g -cp dist/sim-release.jar
     
     scala> import org.goobs.sim._
     scala> implicit val wordnet = Ontology.load("aux/ontology.ser.gz")
     scala> implicit val distsim = DistSim.load("aux/distsim.ser.gz")
     scala> val sim = Similarity("cat", "dog")
     scala> println(sim.lesk)
     scala> println(sim.cos)

For Java users, the `aux/JavaExample.java` file has a minimal Java program
which uses the resource. It can be compiled and run with (from the project root):

     $ javac -cp dist/sim-release.jar aux/JavaExample.java
     $ java  -cp dist/sim-release.jar:aux -Dwordnet.database.dir=aux/WordNet-3.1/dict -mx4g JavaExample

Visualization
------
![Similarity Visualization](https://graphics.stanford.edu/wikis/cs448b-12-fall/A3-SukolsakSakshuwongAngeliGabor?action=AttachFile&do=get&target=screenshot2.png "Similarity Visualization")

A visualization for various similarity metrics is included, in large part
courtesy of Sukolsak Sakshuwong (github: `sukolsak`).
To start the server, run `viz-server` from the project root.
This will load the similarity server, as well as a simply Python server
on port 8000.
Navigating to localhost:8000 should start up the interface.

Three words should be chosen as the "basis" words -- the distances between these
words will be accurate according to the similarity metric chosen.
Additional words can be added, and will be displayed within the barycentric
coordinates defined by the basis.
That is, the distance from the added word to each of the three basis words will be
accurate, but the distance between the added words may not be.
If possible, a wordnet hierarchy is shown to in the right pane.


License
-----
This library is released under the FreeBSD (2-clause) license, as of 2014 onwards.

