package lab1;

/**
 * Printeaza toate argumentele trimise de la linia de comanda.
 * Acestea sunt pasate metodei main prin intermediul sirului de
 * stringuri "args".
 *
 * Putem itera args prin mai multe metode:
 *  - fie folosind un index i pana la lungimea vectorului args
 *  - fie folosind constructia for-each.
 *
 * Putem compila de la linia de comanda clasa folosind:
 *      javac ArgumentsPrinter.java
 * Pentru a rula, folosim:
 *      java ArgumentsPrinter [lista_de_argumente], de exemplu:
 *      java ArgumentsPrinter 1 2 abc 2.3
 */
public class ArgumentsPrinter {

    public static void main(String[] args) {
        System.out.println("Au fost trimise " + args.length + " argumente");

        for(int i = 0; i < args.length; ++i) {
            System.out.print(args[i] + " ");
        }

        // Sau putem itera prin argumente folosind for-each.
        for(String s : args) {
            System.out.println(s);
        }
    }
}
