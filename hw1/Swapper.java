public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        // TODO: Implement me!
	int size = interval.getY() - interval.getX();
	for(int i = 0; i < size + 1; i++){
		buffer[i + offset] = content.charAt(i + interval.getX());
	} 
    }
}
