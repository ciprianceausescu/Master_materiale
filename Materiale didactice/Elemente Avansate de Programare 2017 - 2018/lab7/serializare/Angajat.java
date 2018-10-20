package lab9.serializare;

import java.io.Serializable;

/**
 * Consideram clasa Angajat care implementeaza interfata Serializable.
 * Numai obiectele care implementeaza aceata interfata pot fi serializate.
 */
public class Angajat implements Serializable {
    String nume;
    int varsta;
    int salariu;
    static String firma;

    public Angajat(String n, int v, int s) {
        nume = n;
        varsta = v;
        salariu = s;
    }

    public void print() {
        System.out.println("Firma:\t\t" + firma + "\n" +
                "Nume:\t\t" + nume + "\n" +
                "Varsta:\t\t" + varsta + "\n" +
                "Salariul:\t" + salariu + "\n");
    }
}
