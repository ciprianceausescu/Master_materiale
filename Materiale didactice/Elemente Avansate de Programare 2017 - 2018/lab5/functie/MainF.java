package lab6_lab7.functie;

/**
 * Exemplul de la pagina 10, din laboratorul 5.
 * Interfețele permit, de asemenea, trimiterea metodelor ca parametrii.
 */
class MainF {

    static double cheamaF(Functie ob, double param) {
        return ob.f(param);
    }

    public static void main(String arg[]) {
        System.out.println(cheamaF(new F1(1, 1, 1), 2));
        System.out.printf("%.2f", cheamaF(new F2(), Math.PI / 2));

        System.out.println();
        Functie cos = new Functie() {
            @Override
            public double f(double x) {
                return Math.cos(x);
            }
        };

        System.out.println(cheamaF(cos, Math.PI / 4));


    }
}
