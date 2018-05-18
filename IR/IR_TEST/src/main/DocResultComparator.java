package main;

import java.util.Comparator;

/**
 * Created by Ciprian Mihai on 5/01/2018.
 */
public class DocResultComparator implements Comparator<DocResults> {

    @Override
    public int compare(DocResults o1, DocResults o2) {
        return (int) (o1.getDocScore() - o2.getDocScore());
    }
}
