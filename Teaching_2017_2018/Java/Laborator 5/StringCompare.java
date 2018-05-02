import java.util.Comparator;

public class StringCompare implements Comparator<Pair> {

    @Override
    public int compare(Pair p1, Pair p2) {
        return p1.getS().compareTo(p2.getS());
    }

}
