package lab9.streams;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Scanner;

/**
 * Duplicam continutul unui fisier in alt fisier.
 */
public class FileDuplicator {

    public static void main(String[] args) throws IOException {
        Scanner sc = new Scanner(System.in);
        FileInputStream fis = null;
        FileOutputStream fos = null;

        try {
            System.out.println("File to copy: ");
            String fileName = sc.next();
            fis = new FileInputStream(fileName);

            System.out.println("Destination file: ");
            fileName = sc.next();
            fos = new FileOutputStream(fileName);

            int c;
            while ((c = fis.read()) != -1) {
                fos.write(c);
            }

            System.out.println("Done!");
        } catch (FileNotFoundException exc) {
            System.out.println("File not found, exiting...");
        } finally {
            if(fis != null) {
                fis.close();
            }
            if(fos != null) {
                fos.close();
            }
        }
    }
}
