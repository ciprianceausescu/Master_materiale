package Orientat_Pe_Conexiune;
import java.net.*;
import java.io.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Clientul trimite 2 valori catre Server si asteapta primirea maximului
 * dintre cele doua
 * 
 * 
 * Mai intai se porneste serverul, apoi se porneste clientul
 * @author CeachiBogdan
 */
public class MyClient {
    
    private static final int SEVER_PORT = 2003;
    private static String ADRESA  = "127.0.0.1";
    // Trimite datele catre server
    public static void trimiteDateCatreServer(DataOutputStream out, String sir) throws IOException {
        
        out.writeUTF(sir);
        out.flush();// goleste sirul din flux
        System.out.println("Am trimis catre server : " + sir);
    }
    
    // Primeste datele de la server
    public static String primesteDateDeLaServer(DataInputStream in) throws IOException {
        String sir = in.readUTF(); //obtine reaspunsul de la server
        System.out.println("Am primit de la server : " + sir);
        return sir;
    }
    
    public static void main(String[] args) {
        // declararea datelor locale
        DataInputStream in = null;
        DataOutputStream out = null;
        Socket socket = null;
        
        try {
            socket = new Socket(ADRESA, SEVER_PORT);
            
            //afisarea unui mesaj de success
            System.out.println("Ne-am conectat la server.");
            // obtinerea unui flux de intrare
            in = new DataInputStream
                    (new BufferedInputStream(socket.getInputStream()));
            // obtinerea unui flux de iesire
            out = new DataOutputStream
                        (new BufferedOutputStream(socket.getOutputStream()));
            
            
            
        } catch (IOException ex) {
           System.err.println("Eroare la conectare " + ex);
           System.exit(1);
        }
        
        // declararea altor variabile auxiliare
        double nr1 = 0.0, nr2 = 0.0;
        BufferedReader citireTastatura;
        String linie;
        
        try {
            // obtinerea unui flux de intrare standard (tastatura)

            citireTastatura = new BufferedReader(new InputStreamReader(System.in));
            System.out.flush();

            //citirea primului numar de la tastatura
            System.out.print("Introduceti primul numar : " );
            linie = citireTastatura.readLine();
            Double tmp = Double.valueOf(linie);
            nr1 = tmp.doubleValue();
            System.out.flush();
            
            // citirea celuil de-al doilea numar
            System.out.println("Introduceti al doilea numar: ");
            linie = citireTastatura.readLine();
            tmp = Double.valueOf(linie);
            nr2 = tmp.doubleValue();
            citireTastatura.close(); // inchiderea intrarii standard
        }catch(IOException ex) {
            System.err.println("Ati citit gresit de la tastatura: " + ex);
        }
        
        // comunicam cu SERVERUL
        String rezultat = "";
        
        try {
            //trimiterea datelor
            trimiteDateCatreServer(out, Double.toString(nr1));
            trimiteDateCatreServer(out, Double.toString(nr2));
            
            //primirea rezultatului
            rezultat = primesteDateDeLaServer(in);
            
            
        }catch(IOException ex) {
            System.err.println(
                        "Eroare la trimitere/primire date : " + ex);
        }
        
        // afisam rezultatul primit
        System.out.println("Serverul a trimis ca maximul dintre cele doua "
                + "introduse este : " + rezultat);
        
    }
    
}
