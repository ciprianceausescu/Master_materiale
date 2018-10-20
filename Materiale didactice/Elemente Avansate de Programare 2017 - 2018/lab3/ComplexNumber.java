package lab3;

/**
 * Encapsuleaza date despre un numar complex.
 */
public class ComplexNumber {
    /**
     * Partea imaginara
     */
    private int im;

    /**
     * Partea reala
     */
    private int re;


    public ComplexNumber(int re, int im) {
        this.re = re;
        this.im = im;
    }

    public ComplexNumber(int re) {
        this(re, 0);
    }

    public void sum(ComplexNumber c) {
        this.re += c.re;
        this.im += c.im;
    }

    @Override
    public String toString() {
        return "(" + re + ", " + im + ")";
    }

    public static ComplexNumber conjugat(ComplexNumber c) {
        return new ComplexNumber(c.re, (-1) * c.im);
    }

    public static ComplexNumber sum(ComplexNumber c1, ComplexNumber c2) {
        return new ComplexNumber(c1.re + c2.re, c1.im + c2.im);
    }
}
