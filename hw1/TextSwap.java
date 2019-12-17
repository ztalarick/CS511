import java.io.*;
import java.util.*;

public class TextSwap {

    private static String readFile(String filename) throws Exception {
        String line;
        StringBuilder buffer = new StringBuilder();
        File file = new File(filename);
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null) {
            buffer.append(line);
        }
        br.close();
        return buffer.toString();
    }

    private static Interval[] getIntervals(int numChunks, int chunkSize) {
        // TODO: Implement me!
	//returns array of the start and end indexes of the chunks 
	
	Interval result[] = new Interval[numChunks];
	int filesize = numChunks * chunkSize;
	for(int i = 0; i < numChunks; i++){
		result[i] = new Interval(i * chunkSize, i * chunkSize + chunkSize - 1);
	}	
        return result;
    }

    private static List<Character> getLabels(int numChunks) {
        Scanner scanner = new Scanner(System.in);
        List<Character> labels = new ArrayList<Character>();
        int endChar = numChunks == 0 ? 'a' : 'a' + numChunks - 1;
        System.out.printf("Input %d character(s) (\'%c\' - \'%c\') for the pattern.\n", numChunks, 'a', endChar);
        for (int i = 0; i < numChunks; i++) {
            labels.add(scanner.next().charAt(0));
        }
        scanner.close();
        // System.out.println(labels);
        return labels;
    }

    private static char[] runSwapper(String content, int chunkSize, int numChunks) {
        List<Character> labels = getLabels(numChunks);
        Interval[] intervals = getIntervals(numChunks, chunkSize);
        // TODO: Order the intervals properly, then run the Swapper instances.
	String alphabet = "abcdefghijklmnopqrstuvwxyz";
	char[] buffer = new char[chunkSize * numChunks];

	ArrayList<Thread> threads = new ArrayList<>();
	for(int i = 0; i < labels.size(); i++){
		Swapper swap = new Swapper(intervals[alphabet.indexOf(labels.get(i))], content, buffer, i * chunkSize);
		Thread t = new Thread(swap);
		t.run();
		threads.add(t);
	}
	boolean flag = false;
	do{
		for(int j = 0; j < threads.size(); j++){
			flag = threads.get(j).isAlive() && flag;
			
		}
	}while(flag);

        return buffer;
    }

    private static void writeToFile(String contents, int chunkSize, int numChunks) throws Exception {
        char[] buff = runSwapper(contents, chunkSize, contents.length() / chunkSize);
        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        writer.print(buff);
        writer.close();
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java TextSwap <chunk size> <filename>");
            return;
        }
        String contents = "";
        int chunkSize = Integer.parseInt(args[0]);

	File file = new File(args[1]);
	long fileSize = file.length();
		
	if(fileSize / chunkSize > 26){	//if number of chunks is > 26
		System.out.println("Chunk size too small");
		return;
	}
	if(fileSize % chunkSize != 0){ //if file size is not a multiple of chunk size
		System.out.println("File size must be a multiple of the chunk size");
		return;
	}

        try {
            contents = readFile(args[1]);
            writeToFile(contents, chunkSize, contents.length() / chunkSize);
        } catch (Exception e) {
            System.out.println("Error with IO.");
		System.out.println(e);
            return;
        }
    }
}
