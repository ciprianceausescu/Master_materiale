package lab11;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class ExThread1 extends Thread {

    private Scanner input;
    private String name;
    private int x;

    public ExThread1(String input, String name, int x) throws FileNotFoundException {
        System.out.println("Constructor thread ExThread1");
        this.input = new Scanner(new File(input));
        this.name = name;
        this.x = x;
    }

    @Override
    public void run() {
        System.out.println("Starting thread " + name);
        while(input.hasNextLine()) {
            System.out.println(input.nextLine());
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        x++;
        System.out.println(x);
        System.out.println("Finishing thread " + name);
    }
}
