package lab8.collections;

/**
 * O clasa Person.
 */
public class Person {

    int varsta;
    String nume;

    public Person(int varsta, String nume) {
        this.varsta = varsta;
        this.nume = nume;
    }

    @Override
    public String toString() {
        return "Person{" +
                "varsta=" + varsta +
                ", nume='" + nume + '\'' +
                 '}';
    }
}
