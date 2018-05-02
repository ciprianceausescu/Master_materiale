import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

public class Main {

    public static void main(String[] args) {

        SortedVector<Pair> list = new SortedVector<>(new IntegerCompare());

        list.add(new Pair(15, "test"));
        list.add(new Pair(21, "test2"));
        list.add(new Pair(5, "test3"));
        list.add(new Pair(17, "test4"));
        Collections.sort(list, list.getComparator());

        System.out.println("BY INTEGER");
        for(Pair p: list)
        {
            System.out.println(p);
        }

        SortedVector<Pair> list2 = new SortedVector<>(new StringCompare());

        list2.add(new Pair(15, "test"));
        list2.add(new Pair(21, "test3"));
        list2.add(new Pair(5, "test6"));
        list2.add(new Pair(17, "test4"));
        Collections.sort(list2, list2.getComparator());

        System.out.println("BY STRING");
        for(Pair p: list2)
        {
            System.out.println(p);
        }
    }
}
