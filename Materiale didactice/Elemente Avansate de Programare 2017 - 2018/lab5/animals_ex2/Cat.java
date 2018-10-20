package lab6_lab7.animals_ex2;

public class Cat implements Animal {

    private String name;

    public Cat(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void eat(Food food) {
        if (!(food instanceof CatFood)) {
            System.out.println(food.getName() + " is not cat food");
            return;
        }

        System.out.println("Ate " + food.getName() + " for " + food.getCalories() + " calories");
    }
}













