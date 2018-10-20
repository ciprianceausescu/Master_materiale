package lab9.streams;

import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.IOException;

/**
 * Scriem la iesirea standard ceea ce citim dintr-un fisier.
 */
public class DISExample {

    public static void main(String[] args) {
        DataInputStream dis = null;

        try {
            dis = new DataInputStream(new FileInputStream("out.dat"));
            int n = dis.readInt();
            System.out.println("n = " + n);
            for(int i = 0; i < n; ++i) {
                System.out.println(dis.readDouble() + "\t");
            }
            System.out.println();

        } catch (IOException exc) {
            System.out.println("IOException occurred: "+ exc.getMessage());
        } finally {
            try {
                if(dis != null) {
                    dis.close();
                }
            } catch (IOException e) {
                System.out.println("Error closing the file..");
            }
        }
    }
}
