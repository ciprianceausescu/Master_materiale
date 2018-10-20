package tablouri;

/**
 * Exemplu de array multidimensional.
 * Un array multidimensional poate fi privit ca un array
 * pentru care elementele sale sunt tot niste array-uri.
 */
public class MultidimensionalArray {

    public static void main(String args[]) {

        int[][] a = new int[3][3];
        for(int i = 0; i < 3; ++i) {
            for(int j = 0; j < 3; ++j) {
                a[i][j] = i + j;
            }
        }

        for(int i = 0; i < 3; ++i) {
            for(int j = 0; j < 3; ++j) {
                System.out.print(a[i][j] + " ");
            }
            System.out.println();
        }

        int[][] b = new int[3][];
        b[0] = new int[2];
        for(int i = 0; i < 2; ++i) {
            b[0][i] = i * 2 + 3;
        }

        b[1] = new int[4];
        for(int i = 0; i < 4; ++i) {
            b[1][i] = i + 1;
        }

        b[2] = new int[3];
        for(int i = 0; i < 3; ++i) {
            b[2][i] = (i + 3) * 2;
        }

        System.out.println();
        for(int i = 0; i < 3; ++i) {
            int length = b[i].length;
            for(int j = 0; j < length; ++j) {
                System.out.print(b[i][j] + " ");
            }
            System.out.println();
        }

    }
}
