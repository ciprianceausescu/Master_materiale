
package exemple;

import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * 
 * 
 * PRODUCATOR ----> BUFFER -----> CONSUMATOR
 * Doua threaduri comunica prin intemrmediul unui buffer (memorie partajata):
 *  - thread-ul Producator creaza datele si le pune in buffer
 *  - thread-ul Consumator ia datele din buffer si le prelucreaza
 * Probleme de coordonare:
 *  1. Producatorul si consumatorul nu vor accesa bufferul simultan.
 *  2. Producatorul nu va pune in buffer date noi daca datele din buffer nu au 
 * fost consumate.
 *  3. Cele doua thread-uri se vor anunta unul pe altul cand starea buferului 
 * s-a schimbat
 * 
 * Modelul Producator Consumator
 * implementarea foloseste blocuri cu garzi
 * thread-ul este suspendat pana cand o anume conditie este satisfacuta
 *
 *
 *
 *Guarded blocks ( folosit cand vrei sa faci threadurile sa se coordoneze) :
 *https://docs.oracle.com/javase/tutorial/essential/concurrency/guardmeth.html
 *
 * @author CeachiBogdan
 */

class Buffer { // implementarea Bufferului, acesta se face cu metode sincronizate
    private String message;
    private boolean empty = true;
    
    public synchronized String take() { // accesul se face prin metode sincronizate
        while(empty) { 
            try {
                wait(); // Object.wait  to suspend the current thread 
                
                /*
                 * The invocation of wait does not return until 
                 * another thread has issued a notification that some special event may have occurred
                 *  — though not necessarily the event this thread is waiting for:
                 */
            } catch (InterruptedException ex) {
                System.out.println(ex.getMessage());
            }
        }
        
        empty = true;
        notifyAll();
        return message;
    }
    public synchronized void put(String message) {
        while(!empty) {
            try {
                wait();
            } catch (InterruptedException ex) {
               System.out.println(ex.getMessage());
            }
        }
        
        System.out.println("Producer put  in the buffer message  = " + message);
        empty = false;
        this.message = message; // pune mesajul
        notifyAll(); // anunta
    }
}
class Producer implements Runnable {
    private Buffer buffer;
    public Producer(Buffer buffer) {
        this.buffer = buffer;
    }

    @Override
    public void run() {
        String importantInfo[] = {
            "Mares ear oats",
            "Does eat oats",
            "Little lambs eat ivy",
            "A kid will eat ivy too"
        };
        Random random = new Random();
        
        for (int i = 0; i < importantInfo.length; i++) {
            try {
                buffer.put(importantInfo[i]);
                
                Thread.sleep(random.nextInt(5000));
            } catch (InterruptedException ex) {
                System.out.println(ex.getMessage());
            }
        }
        buffer.put("DONE");
    }
}
class Consumer implements Runnable {
    private Buffer buffer;
    public Consumer(Buffer buffer) {
        this.buffer = buffer;
    }

    @Override
    public void run() {
        Random random = new Random();
        for(String message = buffer.take(); ! message.equals("DONE"); message = buffer.take()) {
            System.out.format("Consumer takes the message =  %s%n", message);
            try {
                Thread.sleep(random.nextInt(5000));
            } catch (InterruptedException ex) {
                System.out.println(ex.getMessage());
            }
        }
    }
}








public class Exemplu5_Producer_Consumer {
    
    public static void main(String[] args) {
        
        Buffer buffer = new Buffer();
    
        Thread t1 = new Thread(new Producer(buffer));
        Thread t2 = new Thread(new Consumer(buffer));
        
        t1.start();
        t2.start();
        
    }
}


/*
OUTPUT:
MESSAGE RECEIVED Mares ear oats
MESSAGE RECEIVED Does eat oats
MESSAGE RECEIVED Little lambs eat ivy
MESSAGE RECEIVED A kid will eat ivy too
*/
