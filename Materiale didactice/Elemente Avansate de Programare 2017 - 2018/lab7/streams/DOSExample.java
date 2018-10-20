package lab9.streams;

import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Scanner;

/**
 * Scriem intr-un fisier ceea ce citim de la tastatura.
 */
public class DOSExample {

    public static void main(String[] args) {

        Scanner sc = new Scanner(System.in);
        System.out.print("n = ");
        int n = sc.nextInt();
        DataOutputStream dos = null;

        try {
            dos = new DataOutputStream(new FileOutputStream("out.dat"));
            dos.writeInt(n);
            System.out.println("The n numbers are: ");
            for(int i = 0; i < n; ++i) {
                dos.writeDouble(sc.nextDouble());
            }
        } catch (IOException exc) {
            System.out.println("IOException  occurred: " + exc.getMessage());
        } finally {
            try {
                if(dos != null) {
                    dos.close();
                }
            } catch (IOException e) {
                System.out.println("Error closing the file...");
            }

        }
    }
}
