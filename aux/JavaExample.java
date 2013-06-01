
import org.goobs.sim.*;

public class JavaExample {
  public static void main(String[] args) {
    // Wordnet Similarity
    Ontology ontology = Ontology.load("aux/ontology.ser.gz");
    // Get the similarity between cat and dog.
    // Make sure that both terms exist in the ontology with ontology.contains()
    // or else the resulting similarity will be meaningless.
    Ontology.Similarity sim = ontology.sim("cat", "dog");
    System.out.println("Lesk similarity between cat and dog is: " + sim.lesk());
    
    // Distributional Similarity
    DistSim distsim = DistSim.load("aux/distsim.ser.gz");
    // If both terms are UNK, no similarity is returned (and .get() will crash).
    DistSim.Similarity sim2 = distsim.sim("cat", "dog").get();
    System.out.println("Cos similarity between cat and dog is: " + sim2.cos());
  }
}
