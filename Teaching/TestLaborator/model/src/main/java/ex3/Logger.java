package ex3;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

public class Logger implements AutoCloseable {
	private File file;
	private FileOutputStream fos;
	private static Integer nrMesaj = 0;

	public Logger() throws FileNotFoundException {
		file = new File("log.txt");
		fos = new FileOutputStream(file);
	}

	public synchronized void write(String message) throws IOException {
		String m = nrMesaj++ + message + "\n";
		fos.write(m.getBytes());

	}

	@Override
	public void close() throws Exception {
		fos.close();
	}

}
