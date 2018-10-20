package lab11;

import java.io.FileNotFoundException;

public class ExMain {

    public static void main(String[] args) throws FileNotFoundException, InterruptedException {

        ExThread1 thread1 = new ExThread1("inputs/input1.txt", "Thread1", 10);
        thread1.start();

        // Mai intai ruleaza primul thread si apoi incepe cel de-al doilea
        thread1.join();

        ExThread1 thread2 = new ExThread1("inputs/input2.txt", "Thread2", 10);
        thread2.start();

    }
}
