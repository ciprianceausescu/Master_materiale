
package Fisiere;

import java.io.*;

public class FisierCaracter {
    public static void main(String args[])
    {
        try(FileReader fin=new FileReader("Test.in"))
        {
            int x;
          
            while((x=fin.read())!=-1)
                System.out.print((char)x);
         
        }catch(FileNotFoundException er)
        {
            System.err.println("Fisier de intrare inexistent!");
        }
        catch(IOException er)
        {
             System.err.println("Eroare la citirea in fisier!");
        }
    }
    
}
