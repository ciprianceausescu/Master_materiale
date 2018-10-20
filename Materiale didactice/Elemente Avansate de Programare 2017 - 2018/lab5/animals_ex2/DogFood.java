package lab6_lab7.animals_ex2;

public class DogFood implements Food {

    @Override
    public int getCalories() {
        return 100;
    }

    @Override
    public String getName() {
        return "dogfood";
    }
}
