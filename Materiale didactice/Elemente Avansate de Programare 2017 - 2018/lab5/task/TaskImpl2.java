package lab6_lab7.task;

/**
 * Created by nicoleta on 4/1/2015.
 */
public class TaskImpl2 implements Task {

    String message;

    public TaskImpl2(String message) {
        this.message = message;
    }

    @Override
    public void execute() {
        System.out.println(message);
    }
}
