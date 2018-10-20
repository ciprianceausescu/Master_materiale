package lab6_lab7.task;

public class TaskMain {


    public static void main(String[] args) {

        Task task = new TaskImpl1();
        task.execute();

        task = new TaskImpl2("abc");
        task.execute();

        task = new TaskImpl3();
        task.execute();

        task = new TaskImpl3();
        task.execute();
    }
}
