
package exemple;

/**
 *Exemplu simplu de a crea un thread cu Runnable
 * @author CeachiBogdan
 */




class MyClassRunnable implements Runnable {
    
    public void run() {
        try {
        for(int i = 0; i < 50; i += 20) {
            System.out.println(i);
            Thread.sleep(50); //blocheaza firul de executie
        }
        }catch(InterruptedException e) {
            //orice comportament care presupune blocare a unui fir de 
            //executie arunca InterruptedException
            e.printStackTrace();
        }
    }
}
public class Exemplu_Runnable {
    
      public static void main(String[] args) {
          
        MyClassRunnable r1 = new MyClassRunnable();
        
        // cele doua threaduri se folosesc de aceiasi instanta =>
        //deci au aceleasi atribute
        Thread t1 = new Thread(r1);
        Thread t2 = new Thread(r1);
        t1.start();
        t2.start();
        
        try {
            t1.join();
            t2.join();
        }catch(InterruptedException e) {
            e.printStackTrace();
        }
    }
}
