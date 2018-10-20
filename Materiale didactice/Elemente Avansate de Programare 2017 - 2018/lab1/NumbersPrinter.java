package lab1;

/**
 * Printeaza toate numerele trimise la linia de comanda.
 * Amintim:
 *  - argumentele primite de metoda main sunt o lista de stringuri.
 *  - valorile numerice trebuie convertite explicit din stringuri in numere
 *  - acest lucru se poate realiza prin intermediul unor metode de tipul
 *    parseTipNumeric din clasa infasuratoare a tipului la care vrem sa facem
 *    conversia
 *  - clasele infasuratoare sunt:
 *      int     <-> Integer
 *      double  <-> Double
 *      long    <-> Long
 *      short   <-> Short
 *      byte    <-> Byte
 *      boolean <-> Boolean
 *      char    <-> Character
 *  - metodele de tipul Integer.parseInt(sir) arunca exceptia
 *    NumberFormatException, fiind necesara tratarea ei.
 */
public class NumbersPrinter {

    public static void main(String[] args) {
        System.out.println("Au fost trimise " + args.length + " argumente");

        for(String s : args) {
            try {
                int num = Integer.parseInt(s);
                System.out.println(num);
            }catch (NumberFormatException exc) {
                System.out.println("Argumentul " + s + " nu poate fi convertit la un numar");
            }
        }
    }
}
