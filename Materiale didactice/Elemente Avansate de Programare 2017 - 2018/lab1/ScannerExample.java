package lab1;

import java.util.Scanner;

/**
 * Citirea de la tastatura a argumentelor.
 * Putem folosi clasa Scanner, care face parte din pachetul
 * java.util.
 *
 * Contine metode cum ar fi next(), nextInt(), nextDouble(),
 * care citesc urmatorul element din buffer si il intorc ca un
 * tip de date corespunzator (ex. String, int, double respectiv).
 * Pentru a se verifica existenta in buffer a unui element de un
 * anumit tip, se folosesc metode de forma hasNext(), hasNextInt() etc.
 */
public class ScannerExample {

    public static void main(String args[]) {

        Scanner scanner = new Scanner(System.in);

        // Citim numarul de argumente
        int numOfElements = scanner.nextInt();

        // Citim argumentele si le afisam la iesirea standard
        for(int i = 0; i < numOfElements; ++i) {
            System.out.println(scanner.next());
        }
    }
}
