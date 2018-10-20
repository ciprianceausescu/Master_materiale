package lab8.collections;

import java.util.Comparator;

/**
 * Un comparator de persoane. Compara mai intai dupa nume,
 * si apoi dupa varsta.
 */
public class PersonComparator implements Comparator<Person> {

    @Override
    public int compare(Person o1, Person o2) {
        if (o1.nume.compareTo(o2.nume) < 0) {
            return -1;
        } else if(o1.nume.compareTo(o2.nume) > 0) {
            return 1;
        }
        return o1.varsta - o2.varsta;
    }
}
