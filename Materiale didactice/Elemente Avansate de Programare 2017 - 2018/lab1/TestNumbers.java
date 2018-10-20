package lab1;

/**
 * Printeaza toate argumentele care sunt fie numere prime,
 * fie patrate perfecte, primite de la linia de comanda.
 */
public class TestNumbers {

    /**
     * O metoda statica ce verifica daca un numar primit ca
     * argument este prim
     * @param num Numarul de verificat
     * @return true daca numarul este prim, false altfel.
     */
    public static boolean isPrime(int num) {
        if(num == 0 || num == 1) {
            return false;
        }

        boolean prime = true;
        for(int i = 2; i <= Math.sqrt(num); ++i) {
            if(num % i == 0) {
                prime = false;
                break;
            }
        }
        return prime;
    }

    /**
     * O metoda statica ce verifica daca un numar este patrat perfect.
     * @param num Numarul de verificat
     * @return true daca argumentul este patrat perfect, false altfel
     */
    public static boolean isPerfectSquare(int num) {
        if(num < 0) {
            return false;
        }

        double x = Math.sqrt(num);
        return x == (int) x;
    }


    public static void main(String args[]) {

        System.out.println("There are " + args.length + " arguments.");

        for(int i = 0; i < args.length; ++i) {
            try {
                int num = Integer.parseInt(args[i]);

                // Observati cum se apeleaza o metoda statica: NumeClasa.metodaStatica(argumente...)
                if (TestNumbers.isPrime(num)) {
                    System.out.println("The argument " + num + " is a prime number!");
                } else {
                    System.out.println("The argument " + num + " is not a prime number");
                }

                if (TestNumbers.isPerfectSquare(num)) {
                    System.out.println("The argument " + num + " is a perfect square!");
                } else {
                    System.out.println("The argument " + num + " is not a perfect square!");
                }
            } catch(NumberFormatException nfe) {
                System.out.println("The argument " + args[i] + " is not an integer!");
            }
        }
    }
}
