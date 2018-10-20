package lab3;

/**
 * O clasa ce contine doar metoda main folosita
 * pentru lucrul cu numere complexe.
 */
public class MainComplexNumber {

    public static void main (String[] args) {
        int length = 5;
        ComplexNumber[] array = new ComplexNumber[length];
        array[0] = new ComplexNumber(7);
        array[1] = new ComplexNumber(2, 3);
        array[2] = new ComplexNumber(5, -1);
        array[3] = ComplexNumber.conjugat(array[1]);
        array[4] = ComplexNumber.sum(array[0], array[2]);

        ComplexNumber res = new ComplexNumber(0);
        for(int i = 0; i < length; ++i) {
            System.out.println(array[i]);
            res.sum(array[i]);
        }

        System.out.println(res);
    }
}
