//Zachary Talarick 10/01/19
//I plegde my Honor that I have abided by the Stevens Honor System.

package Assignment2;
import java.util.*;

public class Exercise{

  public enum WeightPlateSize{
    SMALL_3KG , MEDIUM_5KG , LARGE_10KG
  }

  public enum ApparatusType {
  LEGPRESSMACHINE , BARBELL , HACKSQUATMACHINE , LEGEXTENSIONMACHINE ,
  LEGCURLMACHINE , LATPULLDOWNMACHINE , PECDECKMACHINE ,
  CABLECROSSOVERMACHINE
  }

  private ApparatusType at;
  private Map<WeightPlateSize, Integer> weight; // Can you have int here instead of Integer?
  private int duration;

  private static final Random RANDOM = new Random();
  private static final List<ApparatusType> VALUES = Collections.unmodifiableList(Arrays.asList(ApparatusType.values())); //enums
  // private static final VALUES_SIZE = VALUES.size();

  public Exercise(ApparatusType at, Map<WeightPlateSize, Integer> weight, int duration){
    this.at = at;
    this.weight = weight;
    this.duration = duration;
  }
  public ApparatusType getApparatusType(){
    return this.at;
  }
  public int getDuration(){
    return this.duration;
  }
  public Map<WeightPlateSize, Integer> getWeight(){
    return this.weight;
  }

  public String toString(){
    return "(" + this.at + ", " + this.duration + ", " + this.weight + ")";
  }

  private static Boolean noWeight(Map<WeightPlateSize, Integer> weight){

    int result = 0;
    for(int i : weight.values()){
      result += i;
    }
    return result == 0;
  }

  public static Exercise generateRandom(){

    ApparatusType at = VALUES.get(RANDOM.nextInt(VALUES.size()));

    Map<WeightPlateSize, Integer> weight = new HashMap<>();
    do{
      weight.clear(); //cant overwrite keys with new values in case where this loops might not work
      weight.put(WeightPlateSize.SMALL_3KG, RANDOM.nextInt(11)); //CHANGE THESE WHEN USING THIS FROM GYM CLASS
      weight.put(WeightPlateSize.MEDIUM_5KG, RANDOM.nextInt(11));
      weight.put(WeightPlateSize.LARGE_10KG, RANDOM.nextInt(11));
    }while(noWeight(weight));

    int duration = RANDOM.nextInt(30000) + 1;

    return new Exercise(at, weight, duration);
  }
}
