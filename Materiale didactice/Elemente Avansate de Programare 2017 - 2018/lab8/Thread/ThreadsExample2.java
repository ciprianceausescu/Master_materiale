package lab11;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class ThreadsExample2 implements Runnable {

    private Scanner input;

    public ThreadsExample2(String input) {
        try {
            this.input = new Scanner(new File(input));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void run() {
        while (input.hasNext()) {
            System.out.println(input.nextLine());
        }
    }
}
