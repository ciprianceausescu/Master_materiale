package com.Client_Server.Orientat_Pe_Conexiune;

import java.net.*;
import java.io.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * Serverul primeste 2 numere, calculeaza maximul dintre ele si returneaza 
 * clientului
 * 
 * 
 *  * Mai intai se porneste serverul, apoi se porneste clientul
 * @author CeachiBogdan
 */
public class MyServer {
    
    // stabilim portul serverului, ce poate fii intre 1 si 65535
    // porturile intre 1 si 1024 sunt rezervate, de aplicatii sistem
    private static final  int SERVER_PORT_NUMBER = 2003;
    
    
    public static void trimiteDateCatreClient(DataOutputStream out,
                                              String sir) throws IOException {
        out.writeUTF(sir); // trimite catre client sirul
        out.flush(); // goleste fluxul
        
        // afisarea unui mesaj in consola
        System.out.println("Am trimis catre client : " + sir);
    }
    
    public static String primesteDateDeLaClient(DataInputStream in) throws IOException {
        
        String sir = in.readUTF(); // obtine date de la client
        // si afiseaza in consola
        System.out.println("Am primit de la client : " + sir);
        return sir;
    }
    
    public static void main(String[] args) {
        // declarearea variabilelor
        
        DataInputStream in = null;
        DataOutputStream out = null;
        Socket socket = null;
        ServerSocket server = null;
        
        try {
            // creem Portul pentru server
            server = new ServerSocket(SERVER_PORT_NUMBER);
            
            System.out.println("Asteapta un client...");
            // acceptarea unei conexiuni cu un client
            socket = server.accept();
            System.out.println("S-a stabilit conexiunea cu clientul.");
            
            // obtinerea unui flux de intrare convenabil
            in = new DataInputStream
                        (new BufferedInputStream (socket.getInputStream()));
            
            // obtinerea unui flux de iesire convenabil
            out = new DataOutputStream
                        (new BufferedOutputStream(socket.getOutputStream()));
            
        } catch (IOException ex) {
            Logger.getLogger(MyServer.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        // declararea altor variabile auxiliare
        String sirNumere = "";
        double nr1 = 0.0, nr2 = 0.0;
        double max = 0.0;
        
        try {
            // primeste un numar
            sirNumere = primesteDateDeLaClient(in);
            Double tmp = Double.valueOf(sirNumere);
            nr1 = tmp.doubleValue();
            
            // primirea celuilalt numar
            sirNumere = primesteDateDeLaClient(in);
            tmp = Double.valueOf(sirNumere);
            nr2 = tmp.doubleValue();
            
            //calcularea maximului
            max = (nr1 < nr2) ? nr2 : nr1;
            
            //trimiterea rezultatului
            trimiteDateCatreClient(out, Double.toString(max));
                       
            
        } catch (IOException ex) {
            System.err.println(
            "Eroare la trimitere / primire date: " + ex);
        }
    }
}
