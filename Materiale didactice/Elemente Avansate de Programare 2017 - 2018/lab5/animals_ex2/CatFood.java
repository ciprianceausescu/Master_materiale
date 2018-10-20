
package lab6_lab7.animals_ex2;

public class CatFood implements Food {
    @Override
    public int getCalories() {
        return 10;
    }

    public String getName() {
        return "catfood";
    }

    public void doSomething() {
        System.out.println("did something");
    };
}
