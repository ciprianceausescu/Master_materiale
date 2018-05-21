package com.Exemple;

import java.io.*;
import java.net.*;
import java.util.logging.Level;
import java.util.logging.Logger;
/**
 *
 * @author CeachiBogdan
 */
public class Exemplu2_URL {
    private BufferedReader citireTastatura, fluxIntrare;
    
    public static void main(String[] args) {
        Exemplu2_URL continut = new Exemplu2_URL();
        continut.afisare();
    }
    private void afisare() {
        String sirURL = "";
        String linie = "";
            
        try {
            //obtinerea unui flux de la tastatura
            citireTastatura = new BufferedReader(new InputStreamReader(System.in),1);
            
            sirURL = prompt("Introduceti adreasa URL (ex: " + "http://www.fmi.unibuc.ro/):");
            URL adresaURL = new URL(sirURL);
            
            //creearea unei conexiuni
            URLConnection conexiune = adresaURL.openConnection();
            
            //obtinerea unui flux pentru citierea continutului resurse
            fluxIntrare = new BufferedReader(new InputStreamReader(conexiune.getInputStream()));
            
            //cat timp putem citi cate o linie, o afisam in consola
            while((linie = fluxIntrare.readLine()) != null) {
                System.out.println(linie);
            }
        } catch (MalformedURLException ex) {
            System.err.println("URL gresit : " + sirURL + "\n"  + ex);
            System.exit(2);
        } catch (IOException ex) {
            System.err.println("Eroare la stabilirea conexiunii : " + ex);
            System.exit(1);
        }
    }
    
    
    
    //Citeste o linie de la tastatura
    private String prompt(String mesaj) {
        String raspuns = "";
        
        try {
            System.out.print(mesaj);
            System.out.flush();
            
            //citirea unei linii de la tastatura
            raspuns = citireTastatura.readLine();
        }catch(IOException ex) {
            System.err.println("Eroare la citirea de la tastatura: " + ex);
        }
        return raspuns;
    }
}
