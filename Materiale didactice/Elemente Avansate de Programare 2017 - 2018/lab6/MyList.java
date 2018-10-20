package lab8.collections;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Exercitiul 2, lab.6
 * @param <T>
 */
public class MyList<T> {

    List<T> list;
    int dim;

    public MyList(int dim) {
        this.dim = dim;
        list = new ArrayList<T>(dim);
    }

    public void addToList(T value) throws DuplicateElementException {
        if(list.contains(value)) {
            throw new DuplicateElementException("The list already contains the value " + value);
        }

        list.add(value);
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.println("Set the number of elements in the list: ");
        int dim = sc.nextInt();

        MyList<Integer> myList = new MyList<Integer>(dim);
        int counter = 0;

        System.out.println("Elements: ");
        while(counter < dim) {
            try {
                myList.addToList(sc.nextInt());
                ++counter;
            } catch (DuplicateElementException exc) {
                System.out.println(exc.getMessage());
                System.out.println("Retry ...");
            }
        }
        System.out.println(myList.list);
    }
}
