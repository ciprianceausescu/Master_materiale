package stringuri;

/**
 * Determina substringul unui string aflat între prima și cea de-a doua
 * apariție a unui caracter în stringul respectiv, inclusiv caracterul respectiv.
 */
public class SubString {
    public static String solve(String s, char c) {
        int pos = s.indexOf(c); /* daca nu este gasit caracterul, metoda returneaza -1,
                        altfel returneaza prima pozitie
                        pe care este gasit*/

        if (pos >= 0 ){
            int pos2 = s.indexOf(c, pos + 1 ); /* cauta caracterul c de la pozitia poz+1 */
            if (pos2 >= 0 ) {
                return s.substring(pos, pos2 + 1);
            } else {
                return s.substring(pos);
            }
        }
        return s;
    }

    public static void main(String[] args) {

        System.out.println(SubString.solve("abcdefghcabc", 'c'));
        System.out.println(SubString.solve("abcdefghabc", 'd'));
        System.out.println(SubString.solve("abcdefghcabc", 'm'));
        System.out.println(SubString.solve("abcdefghcabc", 'a'));

    }
}
