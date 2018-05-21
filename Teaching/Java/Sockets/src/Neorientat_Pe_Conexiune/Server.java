
package Neorientat_Pe_Conexiune;
import java.net.*;
import java.io.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * 
 * 
 * Un server care lucreaza cu datagrame trebuie sa parcurga urmatoarele etape:
 * 1) creearea unui obiect de tip DatagramSocket, in care trebuie specificat
 * un numar de port.
 * 2) creearea unui obiect DatagramPacket;
 * 3) cu ajutorul primului obiect creat (DatagramSocket), se stocheaza o
 * datagrama in obiectul de tip DatagramPacket
 * 4) se proceseaza cererea primita de la client
 * 5) se completeaza o noua datagrama
 * 6) se trimite datagrama completata la pasul anterior
 * 
 * 
 * @author CeachiBogdan
 */
public class Server {
    static final int PORT = 2003;
    static DatagramSocket socket;
    
    public static void main(String[] args) {
        try {
            //creearea unui socket UDP la portul 2003
            socket = new DatagramSocket(PORT);
        }catch(IOException ex) {
            System.err.println("Eroare la creearea socketului" +
                                ex.getMessage());
            System.exit(1);
        }
        
        //receptionare datagrame
        DatagramPacket datagramPacket;
        datagramPacket = new DatagramPacket(new byte[1], 1);
        System.out.println("Se asteapta clienti...");
        
        // bucla infinita
        while(true) {
            try {
                //receptionam datagrame
                socket.receive(datagramPacket);
                raspundeCerere(datagramPacket);
            } catch (IOException ex) {
                System.err.println("Eroare la trimiterea datagramei" +
                                    ex.getMessage());
            }
        }
    }
    
    static void raspundeCerere(DatagramPacket cerere) {
        // se obtine un flux de iesire
        ByteArrayOutputStream baos;
        baos = new ByteArrayOutputStream();
        DataOutputStream out = new DataOutputStream(baos);
        
        try {
            // scriem in flux timpul curent in milisecunde

            out.writeLong(System.currentTimeMillis());
        } catch (IOException ex) {
            System.err.println("Nu putem trimite timpul curent" +
                                    ex.getMessage());
        }
        
        //construim raspunsul
        DatagramPacket raspuns;
        // stabilirea mesajului
        byte[] data = baos.toByteArray();
        
        //creearea datagramei
        raspuns = new DatagramPacket(data, data.length,
                                     cerere.getAddress(), cerere.getPort());
        try {
            //trimiterea efectiva a pachetului
            socket.send(raspuns);
        } catch (IOException ex) {
            System.err.println("Nu putem trimite datagrama" + ex.getMessage());
        }
        
    }
}
