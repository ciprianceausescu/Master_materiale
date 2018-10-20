package lab6_lab7.hierarchy;


public class Main {

    public static void main(String[] args) {

        Mammal mammal = new Cat();
        System.out.println(mammal.toString());

        Cat cat = (Cat) mammal;
        cat.meow();

        Cat cat1 = new Cat();
        Animal a = cat1; // upcasting

        Dog d = new Dog();
        d.bark();

        Cat c2 = (Cat) a; // downcasting manual!!!
        if (a instanceof Cat) {
            c2 = (Cat) a;
        }
        c2.meow();

        Animal[] a2 = new Animal[2];
        a2[0] = new Cat();
        a2[1] = new Dog();
    }
}

