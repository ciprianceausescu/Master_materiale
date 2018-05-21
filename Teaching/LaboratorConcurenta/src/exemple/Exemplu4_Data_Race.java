
package exemple;

/**
 * Doua Threaduri vor sa acceseze o resursa concomitent.
 * 
 * Acest lucru poarta denumirea de : Data Race
 * 
 * 
 * https://stackoverflow.com/questions/2120248/how-to-synchronize-a-static-variable-among-threads-running-different-instances-o
 * @author CeachiBogdan
 */
 class Task implements Runnable {
    
    static int counter = 0; // resursa ce o sa fie accesata de mai multe threaduri
    
    @Override
    public void run() {
        for (int i = 0; i < 5; i++) {
            performTask();
        }
    }
    
    synchronized void performTask() { // solutie: se pune synchronized void performTask()
        int temp = counter;
        counter++;
        
        System.out.println(Thread.currentThread().getName() + " before: " +
                          temp + " after : " + counter); 
    }
    
    
    
    
}


public class Exemplu4_Data_Race {
    
    public static void main(String[] args) throws InterruptedException {
        Thread t1 = new Thread(new Task());
        Thread t2 = new Thread(new Task());
        
        t1.start();
        t2.start();
    }
}


/*
in Output se poate observa cum se intercaleaza
*/