
package com.Exemple;
import java.net.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Clasa InetAddress
 * cu ajutorul acestei clase, putem trimite date prin retea,
 * utilizand adresa IP specifica masinii-destinatie.
 * - furnizeaza access abstract la adresele IP.
 * - Nu exista constructori pentru aceasta clasa. Instantele sunt create
 * prin intermediul metodelor statice:
 * getByName(), getLocalHost(), getAllByName()
 * 
 * getLocalHost() - returneaza un obiect InetAddress corespunzator masinii locale
 * getByName(String host) -> pt masina gazda se construieste un obiect InetAddress
 * unde host poate fii specificat prin nume sau adresa IP
 * getByAddress(byte[] addr) intoarce un obiect InetAddress corespunzator
 * adresei IP date in tabloul specificat. pentru IPv4 se trimite un tablou de
 * 4 elemente (octezi) , iar pt IPv6 se trimit 16 elemente
 * 
 * 
 * @author CeachiBogdan
 */
public class Exemplu_AdresaIpLocala {
    
    // Urmatorul program afiseaza adresa IP locala
    
    public static void main(String[] args) {
        InetAddress adresaIPLocala = null;
        
        try {
            adresaIPLocala = InetAddress.getLocalHost();
            System.out.println("adresa IP locala = " + adresaIPLocala);
            
        } catch (UnknownHostException ex) {
            System.err.println("Nu putem gasi gazda: " + ex.toString());
        }
    }
}

/*
    O posibila executie al acestui program :
    adresaIPLocala = DESKTOP-8ERNLD5/192.168.56.1
    unde DESKTOP-8ERNLD5 este numele calculatorului,
    iar 192..  este IP-ul masinii locale

*/
