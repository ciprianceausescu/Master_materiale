package lab8.collections;

import java.util.ArrayList;
import java.util.List;

/**
 * Exercitiul 5, lab. 6
 */
public class SingleElement {

    public static void main(String[] args) {

        List<Integer> list = new ArrayList();
        list.add(1);
        list.add(1);
        list.add(2);
        list.add(2);
        list.add(3);
        list.add(3);
        list.add(3);

        int result = 0;
        for(int x : list) {
            result ^= x;
        }
        System.out.println(result);

    }
}
