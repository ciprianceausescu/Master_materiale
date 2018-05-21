/**
 * Created by Ciprian Mihai on 5/16/2018.
 */
public class HelloCount {
    public static void main(String[] args) {
        HelloThread hello = new HelloThread();
        CountThread count = new CountThread();

        hello.start();
        count.start();
    }
}
class HelloThread extends Thread{
    public void run(){
        int pause;
        for(int i=0;i<=5;i++){
            try{
                System.out.println("Hello!");
                pause = (int)(Math.random()*3000);
                sleep(pause);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }}}}
class CountThread extends Thread{
    public void run(){
        int pause;
        for(int i=0;i<=5;i++)
        {
            System.out.println(i);
            pause = (int)(Math.random()*3000);
            try {
                sleep(pause);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
