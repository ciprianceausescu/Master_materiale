import java.util.Comparator;

public class IntegerCompare implements Comparator<Pair> {

    @Override
    public int compare(Pair p1, Pair p2) {
        if(p1.getI() < p2.getI())
            return -1;
        if(p1.getI() > p2.getI())
            return 1;

        return 0;
    }
}
