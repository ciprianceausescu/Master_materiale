//lista cu numere crescatoare pozitive si lista cu numere negative sortate descrescator
package colectii_date_2014;

import java.util.*;
import java.io.*;
public class lista {
    public static void main(String args[]) throws Exception
    {
        ArrayList lp=new ArrayList();
        ArrayList ln=new ArrayList();
        
        
        Scanner fin=new Scanner(new File("set.txt"));
        
        int x;
        
        while(fin.hasNext())
        {
            x=fin.nextInt();
            if(x>=0) lp.add(x);
            else
                ln.add(x);
        }
        fin.close();
    Collections.sort(lp);
    Collections.sort(ln);
    Collections.reverse(ln);
    
     System.out.print("Lista nr pozitive sortate crescator: ");
    for(int i=0;i<lp.size();i++)
        System.out.print(lp.get(i)+" ");
    System.out.println("\nLista nr pozitive sortate crescator: ");
    for(int i=0;i<ln.size();i++)
        System.out.print(ln.get(i)+" ");
    }
}
