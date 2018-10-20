package lab6_lab7.animals_ex2;

public class Dog implements Animal {

    private String name;

    public Dog(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }
    
    @Override
    public void eat(Food food) {
        System.out.println("Ate " + food.getName());
    }
}
