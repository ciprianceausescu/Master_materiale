import java.io.Serializable;

/**
 * Created by Ciprian Mihai on 5/18/2018.
 */
public class Pisica extends Animal implements Serializable{
    Pisica(int varsta, int greutate, String nume){
        super(varsta, greutate, nume);
    }
    @Override
    void mananca() {
        System.out.println("lapte");
    }
    void miauna(){
        System.out.println("Pisica miauna");
    }
}
