package Threads;

public class DisplayMessage implements Runnable
{
    private String message;
    public DisplayMessage(String message)
    {
        this.message = message;
    }
    public void run()
    {
        System.out.println(message);
    }

    public static void main(String[] args) {
        DisplayMessage r = new DisplayMessage ("Hello, World");
        r.run();
    }
}
