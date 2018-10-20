package lab11;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class ExThread2 implements Runnable {

    private String name;
    private Scanner input;

    public ExThread2(String name, String input) throws FileNotFoundException {
        System.out.println("In constructor ExThread2");
        this.name = name;
        this.input = new Scanner(new File(input));
    }

    @Override
    public void run() {
        System.out.println("Starting thread " + name);
        while(input.hasNextLine()) {
            System.out.println(input.nextLine());
        }
        System.out.println("Finishing thread " + name);

    }
}
