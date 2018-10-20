package lab6_lab7.animals_ex2;

public class MainFood {

    public static void main(String[] args) {
        Food food = new CatFood();
        System.out.println(food);

        if(food instanceof CatFood) {
            CatFood catFood = (CatFood)food;
            catFood.doSomething();
        }

        CatFood catFood = new CatFood();
        System.out.println(catFood.getName());
        catFood.doSomething();

        Food food2 = new DogFood();
        Dog dog = new Dog("Azorel");
        dog.eat(food2);

        Cat cat = new Cat("Mimi");
        cat.eat(food);
        cat.eat(food2);
        cat.eat(catFood);

        Food food3 = new Food() {
            @Override
            public int getCalories() {
                return 10;
            }

            @Override
            public String getName() {
                return "abc";
            }
        };
        System.out.println(food3.getCalories());
    }
}
