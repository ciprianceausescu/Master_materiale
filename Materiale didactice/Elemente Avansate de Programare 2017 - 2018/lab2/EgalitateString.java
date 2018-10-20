package stringuri;

/**
 * Egalitatea stringurilor.
 * Amintim: Operatorul == testează dacă dacă două referințe
 * indică același obiect. În particular, dacă s1 și s2 sunt
 * două variabile de tip String, atunci s1==s2 este true doar
 * dacă s1 și s2 sunt referințe către același șir (nu dacă
 * șirurile sunt egale în sens lexicografic).
 *
 * Pentru a testa egalitatea se folosește metoda "equals"
 * (sau "equalsIgnoreCase" pentru a nu diferenția literele mari
 * de cele mici).
 */

public class EgalitateString {

    public static void main(String[] args) {

        String s1 = "ab";
        char c1 = 'a';
        char c2 = 'b';
        String s2 = c1 + "" + c2;
        System.out.println("s1 = " + s1);
        System.out.println("s2 = " + s2);

        System.out.println(s1 == s2);
        System.out.println(s1.equals(s2));
    }
}
