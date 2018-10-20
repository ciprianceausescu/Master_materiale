package lab11;

public class MainThreads {

    public static void main(String args[]) {

        ThreadsExample ex1 = new ThreadsExample("inputs/input1.txt");
        ThreadsExample ex2 = new ThreadsExample("inputs/input2.txt");
        ex1.start();
        ex2.start();

//        ThreadsExample2 ex1 = new ThreadsExample2("inputs/input1.txt");
//        Thread thread = new Thread(ex1);
//        thread.start();
//
//        ThreadsExample2 ex2 = new ThreadsExample2("inputs/input2.txt");
//        Thread thread2 = new Thread(ex2);
//        thread2.start();
    }
}
