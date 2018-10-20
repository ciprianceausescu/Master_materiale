
package colectii_date_2014;

import java.io.*;
import java.util.*;

public class multumime {
    public static void main(String args[]) throws Exception
    {
        HashSet multime=new HashSet();
        
        Scanner fin=new Scanner(new File("set.txt"));
        
        int x;
        
        while(fin.hasNext())
        {
            x=fin.nextInt();
            multime.add(x);
        }
        
        fin.close();
        
        Iterator it=multime.iterator();
        PrintWriter fout=new PrintWriter("disticte.txt");
       while(it.hasNext())
           fout.write(it.next()+" ");
       fout.close();
 }
        
    }
    
    

