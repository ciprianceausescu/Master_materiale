package lab11;


import java.io.*;
import java.util.Scanner;

public class ThreadsExample extends Thread {

    private Scanner input;

    public ThreadsExample(String input) {
        super(input);
        try {
            this.input = new Scanner(new File(input));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void run() {
        while (input.hasNext()) {
            try {
                System.out.println(input.nextLine());
                Thread.sleep(50);
            } catch (InterruptedException e) {
                System.out.println("Thread interrupted");
            }
        }
    }
}
