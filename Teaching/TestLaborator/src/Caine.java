/**
 * Created by Ciprian Mihai on 5/18/2018.
 */
public class Caine extends  Animal {
    Caine(int varsta, int greutate, String nume){
        super(varsta, greutate, nume);
    }
    @Override
    void mananca() {
        System.out.println("oase");
    }
    void latra(){
        System.out.println("Cainele latra");
    }
}
