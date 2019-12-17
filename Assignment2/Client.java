//Zachary Talarick 10/01/19
//I plegde my Honor that I have abided by the Stevens Honor System.

package Assignment2;
import java.util.*;

public class Client{
  private int id;
  private List<Exercise> routine;

  private static final Random RANDOM = new Random();

  public Client(int id){
    this.id = id;
    routine = new ArrayList<Exercise>();
  }
  public List<Exercise> getRoutine(){
    return this.routine;
  }
  public int getID(){
    return this.id;
  }

  public void addExercise(Exercise e){
    routine.add(e);
  }
  public String toString(){
    return "(" + this.id + ", " + this.routine + ")";
  }

  public static Client generateRandom(int id){
    Client result = new Client(id);
    int num_exercises = RANDOM.nextInt(5) + 15; //generates numbers between 15 and 20
    for(int i = 0; i < num_exercises; i++){
      result.addExercise(Exercise.generateRandom()); //how to use generateRandom from Exercise
    }
    return result;
  }
}
