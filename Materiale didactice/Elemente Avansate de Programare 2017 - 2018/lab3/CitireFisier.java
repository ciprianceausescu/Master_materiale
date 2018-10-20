package lab3;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

/**
 * Exemplificam citirea din fisier
 * Atentie! Trebuie tratata exceptia FileNotFoundException,
 * altfel vom avea eroare la compilare!
 */
public class CitireFisier {

    public static void main(String[] args) {

        try {
            Scanner scanner = new Scanner(new File("input.txt"));

            // Citim linie cu linie continutul unui fisier
            while(scanner.hasNextLine()) {
                System.out.println(scanner.nextLine());
            }
        } catch (FileNotFoundException e) {
            System.out.println("File not found!");
        }
    }
}
