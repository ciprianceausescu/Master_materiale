
package com.Exemple;
import java.applet.*;
import java.net.*;
import java.awt.*;
import java.awt.event.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Appletul citeste de la tastatura o adresa URL si se intoarce numele
 * protocolului, numele masinii gazda, portul, numele fisierului
 * si referinta(ancora)
 * @author CeachiBogdan
 */
public class Exemplu1_URL extends Applet implements ActionListener {
    //datele membre
    URL url;
    String sirURL;
    Button afiseaza;
    TextField numeCamp;
    
    //initializam appletul
    public void init() {
        //crearea unui buton
        afiseaza = new Button("Introduceti Adresa URL");
        add(afiseaza);
        afiseaza.addActionListener(this);
        //creearea unui camp de text
        numeCamp = new TextField(40);
        add(numeCamp);
    }

    @Override
    public void actionPerformed(ActionEvent event) {
         //luam URL-ul introdus in campul text
        sirURL = numeCamp.getText();
        try {
          //instantiem
            url = new URL(sirURL);
            
            //obtinem numele fisierului
            String numeFisier = url.getFile();
            System.out.println("Numele fisierului este " + numeFisier);
            
            //obtinem numele masinii gazda
            String numeHost = url.getHost();
            System.out.println("Numele masinii gazda " + numeHost);
            
            // obtinem numele portului
            int numarPort = url.getPort();
            System.out.println("Numarul portului" + numarPort);
            
            //numele protocolului
            String numeProtocol = url.getProtocol();
            System.out.println("Numele protocolului este " + numeProtocol);
            
            //extragem referinta (ancora)
            String numeReferinta = url.getRef();
            System.out.println("Numele referintei este " + numeReferinta);
            
        } catch (MalformedURLException ex) {
            System.err.println("Eroare : " + ex.getMessage());
        }
        
    }
}


/*

Exemplu:

http://fmi.unibuc.ro:8080/ro/index.html#admitere
Se afiseaza:
Numele masinii gazda fmi.unibuc.ro
Numarul portului8080
Numele protocolului este http
Numele referintei este admitere
*/
