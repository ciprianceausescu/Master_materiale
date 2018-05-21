
package com.Exemple;
import java.net.*;
import java.io.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *  Urmatorul program afiseaza adresa IP a unei masini la distanta,
 * plecand de la numele de domeniu
 * @author CeachiBogdan
 */
public class Exemplu_AdresaIPDistanta {
    public static void main(String[] args) {
        BufferedReader citireTastatura;
        citireTastatura = new BufferedReader(new InputStreamReader(System.in),1);
        System.out.println("Introduceti un nume de domeniu : ");
        String linie = "";
        
        //citim numele de domeniu ca un String
        
        System.out.flush();
        try {
            linie = citireTastatura.readLine();
            citireTastatura.close();
        } catch (IOException ex) {
            System.out.println("A aparut o exceptie la citirea de la tastatura" 
                                + ex.getMessage());
            System.exit(2);
        }
        
        InetAddress adresaIP = null;
        
        try {
            adresaIP = InetAddress.getByName(linie);
            System.out.println(adresaIP);
            
        } catch (UnknownHostException ex) {
            System.err.println("Nu gasim gazda : " + ex);
        }
        
    }
}

/*
Exemplu:
Introduceti un nume de domeniu : 
fmi.unibuc.ro
fmi.unibuc.ro/193.226.51.6
*/