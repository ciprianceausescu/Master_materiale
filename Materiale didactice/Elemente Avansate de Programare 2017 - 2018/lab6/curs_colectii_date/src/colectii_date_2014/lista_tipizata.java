
package colectii_date_2014;

import java.util.*;
public class lista_tipizata {
    public static void main(String args[])
    {
        ArrayList<Integer> lista=new ArrayList<Integer>();
        
        for(int i=0;i<10;i++)
            lista.add(i);
        Collections.shuffle(lista);
        
        for(ListIterator it=lista.listIterator();it.hasNext();)
        {
            Integer x=(Integer) it.next();
            System.out.print(x+" ");
        }
    }
    
}
