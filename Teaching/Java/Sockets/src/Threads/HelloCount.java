package Threads;

public class HelloCount
{
    public static void main(String[] args)
    {
        HelloThread hello = new HelloThread();
        CountThread count = new CountThread();
        hello.start();
        count.start(); // start este functie predefinita
    }

}

class HelloThread extends Thread {

    public void run() {
        int pause;
        for (int i = 0; i < 5; i++) {
            try {
                System.out.println("Hello!");
                //Again, introduce an element
                //of random
                pause = (int) (Math.random() * 3000);
                sleep(pause);
            } catch (InterruptedException interruptEx) {
                System.out.println(interruptEx);
            }
        }
    }
}

class CountThread extends Thread {

    int pause;

    public void run() {
        for (int i = 0; i < 5; i++) {
            try {
                System.out.println(i);
                pause = (int) (Math.random() * 3000);
                sleep(pause);
            } catch (InterruptedException interruptEx) {
                System.out.println(interruptEx);
            }
        }
    }
}
