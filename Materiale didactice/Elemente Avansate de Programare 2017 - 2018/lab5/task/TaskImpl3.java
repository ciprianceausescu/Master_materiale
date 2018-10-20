package lab6_lab7.task;

/**
 * Created by nicoleta on 4/1/2015.
 */
public class TaskImpl3 implements Task {

    private static int numInstances = 0;

    public TaskImpl3() {
        ++numInstances;

    }

    @Override
    public void execute() {
        System.out.println(numInstances);
    }
}
