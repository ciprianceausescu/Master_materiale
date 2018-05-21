package ex3;

import java.io.FileNotFoundException;

public class Main {
	public static void main(String[] args) throws FileNotFoundException {
		Logger logger = new Logger();
		Worker w1 = new Worker(logger);
		Worker w2 = new Worker(logger);
		Worker w3 = new Worker(logger);
		Worker w4 = new Worker(logger);
		w1.start();
		w2.start();
		w3.start();
		w4.start();

	}
}
