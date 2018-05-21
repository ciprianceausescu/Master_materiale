package exemple;

/**
 *Thread-ul principal(main) creaza un al doilea thread si asteapta ca acesta 
 * sa isi termine executia. Daca al doilea thread nu si-a terminat executia dupa
 * o perioada fixata, thread-ul principal il intrerupe. Threadu-ul secundar,
 * este creat prin implementarea interfetei Runnable
 * @author CeachiBogdan
 */



public class Exemplu2_Intreruperi {

    
    
    private static Runnable runnable = () -> {
            
            
            String importantInfo[] = {
                "Mares eat oats",
                "Does eat oats",
                "Little lambs eat ivy",
                "A kid will eat ivy too"
            };
            try {
                for (int i = 0; i < importantInfo.length; i++) {
                    // Pause for 4 seconds
                    Thread.sleep(4000);
                    // Print a message
                    threadMessage(importantInfo[i]);
                }
            } catch (InterruptedException e) {
                threadMessage("I wasn't done!");
            }
    };
    
    static void threadMessage(String message) {
        String currentThread = Thread.currentThread().getName();
        System.out.println(currentThread + " " + message);
    }
    
    
    public static void main(String[] args) throws InterruptedException {
        long startTime = System.currentTimeMillis();
        long waitingTime = 1000 * 60 * 60; // punel foarte mic si ai sa vezi ca intra pe if-ul de mai jos
        
        
        Thread t1 = new Thread(runnable);
        t1.start();
        
        threadMessage("Waiting for message loop to finish");
        while(t1.isAlive()) {
            threadMessage("Still waiting");
            t1.join(1000);
            
            long currentTime = System.currentTimeMillis();
            if( ((System.currentTimeMillis() - startTime) > waitingTime) && t1.isAlive()) {
                threadMessage("tired of waiting");
                t1.interrupt();
                t1.join();
            }
        }
        
        threadMessage("Finally");
    }
        
}