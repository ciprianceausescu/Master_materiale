package lab9.serializare;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;


/**
 * Deserializarea unui angajat salvat anterior in fisierul
 * serial.txt.
 */
public class Deserializare {

    public static void main(String args[]) {
        FileInputStream fis = null;
        ObjectInputStream ois = null;
        try {
            fis = new FileInputStream("serial.txt");
            ois = new ObjectInputStream(fis);
            Angajat angajat1 = (Angajat) ois.readObject();
            Angajat angajat2 = (Angajat) ois.readObject();
            Angajat.firma = ois.readUTF();
            angajat1.print();
            angajat2.print();

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } finally {
            if (ois != null) {
                try {
                    ois.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}

