package lab6_lab7.task;


import java.text.SimpleDateFormat;

public class TaskImpl1 implements Task {

    String time;

    public TaskImpl1() {
        SimpleDateFormat format = new SimpleDateFormat("HH:mm:ss.SSS");
        time = format.format(System.currentTimeMillis());
    }

    @Override
    public void execute() {
        System.out.println(time);
    }
}
