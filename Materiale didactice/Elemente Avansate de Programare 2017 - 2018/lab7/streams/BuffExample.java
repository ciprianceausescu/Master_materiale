package lab9.streams;

import java.io.*;

/**
 * Exemplu de lucru cu BufferedReader.
 */
public class BuffExample {

    public static void main(String[] args) {
        FileWriter fileWriter = null;
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
            fileWriter = new FileWriter("out.txt");
            String line;
            while((line = br.readLine()) != null) {
                if(line.equals("")) {
                    break;
                }
                fileWriter.write(line + "\n");
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if(fileWriter != null) {
                try {
                    fileWriter.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
