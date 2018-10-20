package stringuri;

/**
 * Valoarea unui șir de caractere de tip String nu poate fi
 * modificată după creare (de exemplu nu se poate modifica un
 * caracter, sau un subșir al șirului). Dacă este necesară și
 * modificarea șirului de caractere se pot utiliza clasele
 * StringBuffer sau StringBuilder din pachetul java.lang
 *
 * Exemplu de lucru cu un StringBuffer.
 * Folosind aceasta clasa, putem modifica stringul prin
 * metode de tipul: append, replace, setCharAt etc.
 */
class StringBufferEx {

    public static void main(String arg[]) {
        String s = "abcdefgh";
        System.out.println("lungime " + s.length());
        System.out.println(s.charAt(2));

        StringBuffer sb = new StringBuffer(s);
        System.out.println(sb);
        sb.setCharAt(2, 'x');
        System.out.println(sb);
        sb.append('y');
        sb.append(1234);
        System.out.println(sb);
        sb.replace(1, 6, "yz");
        System.out.println(sb);
        System.out.println("lungime " + sb.length());
        System.out.println(sb.substring(1, 4));
        s = sb.toString();
        System.out.println(s);

    }
}