/**
 * Created by Ciprian Mihai on 5/16/2018.
 */
public class DisplayMessage implements Runnable{
    String message;
    public DisplayMessage(String message){
        this.message=message;
    }
    @Override
    public void run() {
        System.out.println(message);
    }

    public static void main(String[] args) {
        DisplayMessage dm = new DisplayMessage("Hello, 254");
        dm.run();
    }
}
