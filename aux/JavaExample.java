
import org.goobs.sim.*;

public class JavaExample {
  public static void main(String[] args) {
    // Wordnet Similarity
    Ontology ontology = Ontology.load("aux/ontology.ser.gz");
    Ontology.Similarity sim = ontology.sim("cat", "dog");
    System.out.println("Lesk similarity between cat and dog is: " + sim.lesk());
    
    // Distributional Similarity
    DistSim distsim = DistSim.load("aux/distsim.ser.gz");
    DistSim.Similarity sim2 = distsim.sim("cat", "dog").get();
    System.out.println("Cos similarity between cat and dog is: " + sim2.cos());
  }
}
