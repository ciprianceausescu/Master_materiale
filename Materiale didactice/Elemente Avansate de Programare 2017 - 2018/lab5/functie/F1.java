package lab6_lab7.functie;

/**
 * Functie de gradul 2.
 */
class F1 implements Functie {
    double a, b, c;

    F1(int a1, int b1, int c1) {
        a = a1;
        b = b1;
        c = c1;
    }

    public double f(double x) {
        return a * x * x + b * x + c;
    }
}