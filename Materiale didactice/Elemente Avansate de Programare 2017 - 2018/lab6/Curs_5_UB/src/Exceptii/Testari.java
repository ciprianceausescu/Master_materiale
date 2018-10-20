
package Exceptii;

import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.NoSuchElementException;
import java.util.InputMismatchException;

public class Testari {
    public static void main(String[] args)
    {
        try(Scanner fin=new Scanner(new File("date.in")))
        {
       // Scanner fin=new Scanner(new File("date.in"));
        int a, b, c;
        a=fin.nextInt();
        b=fin.nextInt();
        
        c=a/b;
  
        }catch(FileNotFoundException ob)
        {
            System.out.println("Fisier inexistent!!");
        }catch(InputMismatchException ob)
        {
            System.out.println("Format gresit!");
        }
         catch(ArithmeticException ob)
         {
             System.out.println("Impartire la 0!");
         }
        System.out.println("Continuare");
        
        /*
        
        int a , b; 
        Scanner f=null;
        
        try{
            f=new Scanner(new File("date.in"));
            
            a=f.nextInt();
            b=f.nextInt();
            
            double r;
            r=a/b;
        }catch(FileNotFoundException e)
         {
             System.out.println("Fisier inexistent");       
         }
        catch(InputMismatchException e)
        {
            System.out.println("Val incorecte");
            
        }
        catch(ArithmeticException e)
        {
            System.out.println("Impartire la 0");
        }
        finally
        {
            try{
            f.close();
            }catch(Exception ob)
            {
                System.out.print("Nu se poate inchide");
            }
        }
        
        
        
       
        try(Scanner fin = new Scanner(new File("date.in")))        
        {
        
            a = fin.nextInt();
            b = fin.nextInt();
            
            double c = (double) a / b;
            
            System.out.println(a + " / " + b + " = " + c);
        } 
        catch (InputMismatchException ex)    
        {
            System.out.println(ex);
        }
        catch (FileNotFoundException | ArithmeticException | NoSuchElementException ex)    
        {
            System.out.println(ex);
        }
     
        
     System.out.println("Continuare");
                */
    }
}

    

