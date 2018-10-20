package lab8.collections;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class MainPerson {

    public static void main(String[] args) {

        List<Person> personList = new LinkedList<>();
        personList.add(new Person(30, "ion"));
        personList.add(new Person(20, "vasile"));
        personList.add(new Person(15, "ion"));
        personList.add(new Person(60, "gigel"));
        personList.add(new Person(45, "ion"));
        personList.add(new Person(45, "gigel"));

        Collections.sort(personList, new PersonComparator());
        System.out.println(personList);

    }
}
