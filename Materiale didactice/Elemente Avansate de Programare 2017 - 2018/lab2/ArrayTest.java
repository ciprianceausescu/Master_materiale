package tablouri;

import java.util.Scanner;

/**
 * Citeste un array de la tastatura si afiseaza suma,
 * respectiv produsul elementelor, parcurgand tabloul in
 * doua moduri.
 */
public class ArrayTest {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System. in );
        int n = sc.nextInt();
        int [] a = new int[n];
        for ( int i = 0; i < n; ++i) {
            a[i] = sc.nextInt();
        }

        int sum = 0, prod = 1;

        for ( int i = 0; i < n; ++i) {
            sum += a[i];
        }

        for (int i : a) {
            prod *= i;
        }

        System.out.println("Suma elementelor: " + sum);
        System.out.println("Produsul elementelor: " + prod);
    }
}
