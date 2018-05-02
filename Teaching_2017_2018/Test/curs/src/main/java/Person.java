import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Scanner;

@Getter @Setter @AllArgsConstructor @NoArgsConstructor
public class Person extends PersonParent implements PersonInterface {
    Person (String name, String lastname, int age){
        super(name, lastname);
        this.age = age;
    }
    int age;

    @Override
    public void afisare(){
        switch(age){
            case 20:
                System.out.println("Tanar");
                break;
            case 30:
                System.out.println("Batranel");
                break;
            default:
                System.out.println("Nu este de gasit");
                break;
        }
    }

    public static void main(String[] args) {
        Person p = new Person("Ion", "Costica", 20);
        System.out.println(p.getName());
        p.afisare();
        int x = readNumber();
        int y = readNumber();
        System.out.println(x+y);
    }

    private static int readNumber() {
        Scanner sc = new Scanner(System.in);
        return sc.nextInt();
    }
}
