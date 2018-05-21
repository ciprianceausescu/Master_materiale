package ex3;

import java.io.IOException;

public class Worker extends Thread {
	public Logger logger;

	public Worker(Logger logger) {
		this.logger = logger;
	}

	@Override
	public void run() {
		try {
			logger.write(Thread.currentThread().getName() + " a inceput treaba");
			for (int i = 1; i < 10; i++) {
				System.out.println(1);
			}
			logger.write(Thread.currentThread().getName() + " a termina treaba");
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

}
