package lab9.serializare;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;

/**
 * Serializarea a doi angajati in fisierul serial.txt.
 *
 * Amintim:
 * Serializarea constă în memorarea, într-un anumit format, a stării obiectelor
 * într-un tablou de octeţi sau într-un fişier. Acestea au un caracter serial:
 * pot accepta numai secvenţe de octeţi. Facilitatea ca obiectele să poată
 * "supravieţui" şi după teminarea executării programului care le-a creat se mai
 * numeşte persistenţa datelor.
 *
 * La serializare se transmit numai valorile câmpurilor de instanță ale
 * obiectului, nu și cele statice, care aparțin claselor, nu și instanțelor.
 * De asemenea, la serializarea unui obiect se serializează întregul graf
 * asociat obiectului respectiv (graful asociat unui obiect constă din
 * obiectul respectiv, dar şi din obiectele referite direct sau indirect de el).
 */
public class Serializare {
    public static void main(String[] sir) {
        Angajat.firma = "SRL Serial";
        Angajat angajat1 = new Angajat("Vasile", 25, 1485);
        Angajat angajat2 = new Angajat("Ion", 24, 420);
        FileOutputStream fos = null;
        ObjectOutputStream oos = null;

        try {
            fos = new FileOutputStream("seria.txt");
            oos = new ObjectOutputStream(fos);
            oos.writeObject(angajat1);
            oos.writeObject(angajat2);
            oos.writeUTF(Angajat.firma);

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if(oos != null) {
                try {
                    oos.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            if(fos != null) {
                try {
                    fos.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}

