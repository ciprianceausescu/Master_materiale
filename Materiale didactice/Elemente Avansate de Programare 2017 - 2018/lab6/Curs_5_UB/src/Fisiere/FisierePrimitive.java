
package Fisiere;

import java.util.Scanner;
import java.io.*;
public class FisierePrimitive {
    public static void main(String args[])
    {
        try
        {
            Scanner fin=new Scanner(new File("numere.txt"));
            PrintWriter fout_poz=new PrintWriter("numere_pozitive.txt");
            PrintWriter fout_neg=new PrintWriter("numere_neg.txt");
            
            int x;
            
            
            while(fin.hasNextInt())
            {
                x=fin.nextInt();
                if(x<0)
                    fout_neg.write(x+" ");
                else
                    if(x>0)
                        fout_poz.write(x+" ");
            }
            fin.close();
            fout_neg.close();
            fout_poz.close();
        }catch(FileNotFoundException ob)
        {
            System.err.println("Fisierul nu esxita");
        }
            
    }
    
}

    

