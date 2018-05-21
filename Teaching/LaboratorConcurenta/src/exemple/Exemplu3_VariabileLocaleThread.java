/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package exemple;

/**
 *
 * Variabilele locale ale unui thread
 * 
 * 
 * @author CeachiBogdan
 */


class MyThread implements Runnable {

    private ThreadLocal<Long> id = new ThreadLocal<Long>();
    
    @Override
    public void run() {
        // get the id of the thread
        id.set(Thread.currentThread().getId());
        System.out.println(Thread.currentThread().getName() + " with id = " + id );
        
    }
    
}
public class Exemplu3_VariabileLocaleThread {
    public static void main(String[] args) throws InterruptedException {
        Thread t1 = new Thread(new MyThread());
        Thread t2 = new Thread(new MyThread());
        
        t1.start();
        t2.start();
        t1.join();
        t2.join();
        
    }
}
