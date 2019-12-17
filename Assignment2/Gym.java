//Zachary Talarick 10/01/19
//I plegde my Honor that I have abided by the Stevens Honor System.

//75 of size 10kg, 90 of size 5kg and 110 of size 3kg
package Assignment2;

import java.util.*;
import java.util.concurrent.*;

public class Gym extends Thread{

  private static final int GYM_SIZE = 30;
  private static final int GYM_REGISTERED_CLIENTS = 10000;
  private Map<Exercise.WeightPlateSize, Integer> noOfWeightPlates;
  private Set<Integer> clients; // for generating fresh client ids
  private ExecutorService executor;

  private static final Random RANDOM = new Random();
  // various semaphores - declaration omitted
  private Map<Exercise.ApparatusType, Semaphore> apparati; //Semaphore for each apparatus
  private Semaphore weightPerm;

  public Gym(){
    noOfWeightPlates = new HashMap<Exercise.WeightPlateSize, Integer>();
    noOfWeightPlates.put(Exercise.WeightPlateSize.SMALL_3KG, 110);
    noOfWeightPlates.put(Exercise.WeightPlateSize.MEDIUM_5KG, 90);
    noOfWeightPlates.put(Exercise.WeightPlateSize.LARGE_10KG, 75);

    executor = Executors.newFixedThreadPool(GYM_SIZE);

    clients = new HashSet<Integer>(GYM_REGISTERED_CLIENTS);

    apparati = new HashMap<Exercise.ApparatusType, Semaphore>();
      apparati.put(Exercise.ApparatusType.LEGPRESSMACHINE, new Semaphore(5));
      apparati.put(Exercise.ApparatusType.BARBELL, new Semaphore(5));
      apparati.put(Exercise.ApparatusType.HACKSQUATMACHINE, new Semaphore(5));
      apparati.put(Exercise.ApparatusType.LEGEXTENSIONMACHINE, new Semaphore(5));
      apparati.put(Exercise.ApparatusType.LEGCURLMACHINE, new Semaphore(5));
      apparati.put(Exercise.ApparatusType.LATPULLDOWNMACHINE, new Semaphore(5));
      apparati.put(Exercise.ApparatusType.PECDECKMACHINE, new Semaphore(5));
      apparati.put(Exercise.ApparatusType.CABLECROSSOVERMACHINE, new Semaphore(5));

    weightPerm = new Semaphore(1);
  }
  private int genID(){
    int result = 0;
    do{
      result = RANDOM.nextInt(GYM_REGISTERED_CLIENTS) + 1;
    }while(clients.contains(result));
    return result;
  }
  @Override
  public void run(){
    for(int i = 0; i < GYM_REGISTERED_CLIENTS; i++){
      Client client = Client.generateRandom(this.genID());
      executor.execute(new Runnable() {
        public void run(){
          int size = client.getRoutine().size();
          Boolean properWeight = false;
          for(int j = 0; j < size; j++){
            Exercise currentExercise = client.getRoutine().get(j);
            Exercise.ApparatusType currentApparatus = currentExercise.getApparatusType();
            try{
              apparati.get(currentApparatus).acquire(); //acquire machine


              do { //this is terrible, so inefficient
                weightPerm.acquire(); //acquire weight needed
                if(noOfWeightPlates.get(Exercise.WeightPlateSize.SMALL_3KG) - currentExercise.getWeight().get(Exercise.WeightPlateSize.SMALL_3KG) >=0 &&
                noOfWeightPlates.get(Exercise.WeightPlateSize.MEDIUM_5KG) - currentExercise.getWeight().get(Exercise.WeightPlateSize.MEDIUM_5KG) >= 0 &&
                noOfWeightPlates.get(Exercise.WeightPlateSize.LARGE_10KG) - currentExercise.getWeight().get(Exercise.WeightPlateSize.LARGE_10KG) >= 0){
                  properWeight = true;
                  noOfWeightPlates.replace(Exercise.WeightPlateSize.SMALL_3KG, noOfWeightPlates.get(Exercise.WeightPlateSize.SMALL_3KG) - currentExercise.getWeight().get(Exercise.WeightPlateSize.SMALL_3KG));
                  noOfWeightPlates.replace(Exercise.WeightPlateSize.MEDIUM_5KG, noOfWeightPlates.get(Exercise.WeightPlateSize.MEDIUM_5KG) - currentExercise.getWeight().get(Exercise.WeightPlateSize.MEDIUM_5KG));
                  noOfWeightPlates.replace(Exercise.WeightPlateSize.LARGE_10KG, noOfWeightPlates.get(Exercise.WeightPlateSize.LARGE_10KG) - currentExercise.getWeight().get(Exercise.WeightPlateSize.LARGE_10KG));
                }
                weightPerm.release();
              } while (!properWeight);



              System.out.println("Client " + client.getID() + " is exercising with " + currentExercise);
              Thread.sleep(currentExercise.getDuration()); //similate doing exercise

              weightPerm.acquire(); //return weights back to rack
              noOfWeightPlates.replace(Exercise.WeightPlateSize.SMALL_3KG, noOfWeightPlates.get(Exercise.WeightPlateSize.SMALL_3KG) + currentExercise.getWeight().get(Exercise.WeightPlateSize.SMALL_3KG));
              noOfWeightPlates.replace(Exercise.WeightPlateSize.MEDIUM_5KG, noOfWeightPlates.get(Exercise.WeightPlateSize.MEDIUM_5KG) + currentExercise.getWeight().get(Exercise.WeightPlateSize.MEDIUM_5KG));
              noOfWeightPlates.replace(Exercise.WeightPlateSize.LARGE_10KG, noOfWeightPlates.get(Exercise.WeightPlateSize.LARGE_10KG) + currentExercise.getWeight().get(Exercise.WeightPlateSize.LARGE_10KG));
              weightPerm.release();

              apparati.get(currentApparatus).release();
            } catch (InterruptedException e) {
              e.printStackTrace();
            }

          }
        }
      });
    }
    executor.shutdown();
  }
}
