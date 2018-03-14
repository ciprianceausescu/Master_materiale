import javax.sound.midi.Soundbank;

public class Test {


    public static void main(String[] args) {
        int[]arr = {1,2,3,4};
        call_array(arr[0], arr);
        System.out.println(arr[0] + "," + arr[1] + "," + arr[2] + ", "+arr[3]);
    }
    static void call_array(int i, int arr[]){
        arr[i]=6;
        i=5;
    }
}
