import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

public class SortedVector<E> extends Vector<E> {
    private Comparator comp;

    public Comparator getComparator()
    {
        return comp;
    }

    SortedVector(Comparator comp)
    {
        this.comp = comp;
    }
}
