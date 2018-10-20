/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package colectii_date_2014;

import java.util.*;
import java.io.*;

public class tabele_hash {
    
public static void main(String args[]) throws Exception
{
     Scanner fin=new Scanner(new File("cuvinte.txt"));
     HashMap<String,Integer> tabela=new HashMap<String, Integer>();
     String linie;
     while(fin.hasNextLine())
     {
         linie=fin.nextLine();
         String cuvinte[]=linie.split("[., \n!]+");
         
         for(int i=0;i<cuvinte.length;i++)
             if(tabela.containsKey(cuvinte[i]))
                    tabela.put(cuvinte[i],tabela.get(cuvinte[i])+1);
              else
                 tabela.put(cuvinte[i], 1);
        
         Iterator it=tabela.entrySet().iterator();
         
         //for(Map.Entry<String,Integer> x : tabela.entrySet())
             
             //System.out.println(x.getKey()+" frecenta "+x.getValue());
         System.out.println(tabela);
     }      
}
}