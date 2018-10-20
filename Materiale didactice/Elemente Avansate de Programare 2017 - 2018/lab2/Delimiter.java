package stringuri;

import java.util.Scanner;

/**
 * Imparte o propoziție primită de la tastatură în cuvinte
 * (cuvintele se pot separa prin spațiu, virgulă și punct).
 */
public class Delimiter {

    public static void solve2(String s) {
        Scanner sc = new Scanner(s);
        // useDelimeter foloseste o expresie regulata pentru a specifica unul/mai multi delimitatori
        sc.useDelimiter("[ .,]");
        while (sc.hasNext()) {
            String x = sc.next();
            if (x.length() != 0)
                System.out.println(x);
        }
    }


    public static void main(String[] args) {
        String s = "abc, 1. 5    56,   ,, ... \n, 445.2";
        solve2(s);
    }
}
