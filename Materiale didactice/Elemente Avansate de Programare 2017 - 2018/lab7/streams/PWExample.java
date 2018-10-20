package lab9.streams;

import java.io.IOException;
import java.io.PrintWriter;

/**
 * Exemplu de lucru cu PrintWriter.
 */
class PWExample {
    public static void main(String[] args) {

        PrintWriter out = null;
        try {
            out = new PrintWriter("in.txt");
            out.println(10.0);
            out.println(5);
            out.println("Hello");
        } catch(IOException e) {
            e.printStackTrace();
        } finally {
            if (out != null) {
                out.close();
            }
        }
    }
}