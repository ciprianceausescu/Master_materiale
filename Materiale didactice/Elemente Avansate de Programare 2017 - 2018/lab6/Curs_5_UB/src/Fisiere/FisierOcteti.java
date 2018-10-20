
package Fisiere;

import java.io.*;


public class FisierOcteti {

    public static void main(String[] args)
    {
        try(FileInputStream fin = new FileInputStream("Test.in");
            FileOutputStream fout = new FileOutputStream("copie_octeti.txt"))
        {
            
            int dimFisier = fin.available();
            byte []buffer = new byte[dimFisier];
            
            fin.read(buffer);
            
            fout.write(buffer);
            
        } catch (FileNotFoundException ex)
        {
           System.out.println("Fisier de intrare inexistent!");
        } catch (IOException ex)
        {
            System.out.println("Eroare la scrierea in fisier!");
        }
    }
}
    

