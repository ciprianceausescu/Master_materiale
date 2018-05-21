
package exemple;

/**
 * Exemplu simplu de a crea un thread cu clasa Thread
 * @author CeachiBogdan
 */

class OddNumbersThread extends Thread {
    
    @Override
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


public class Exemplu_ClasaThread {
    public static void main(String[] args) {
        OddNumbersThread t1 = new OddNumbersThread();
        OddNumbersThread t2 = new OddNumbersThread();
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

/*
In acest exemplu trebuie sa observam :
-> ca pot aparea unul dupa altul, dar nu obligatoriu
-> join() -> spune firului sa astepte


Mai adaugati un thread, ce observati?
*/
