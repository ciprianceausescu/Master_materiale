package main;

/**
 * Created by Ciprian Mihai on 5/01/2018.
 */

import java.util.Comparator;

public class DocResultComparator implements Comparator<DocResults> {

    @Override
    public int compare(DocResults o1, DocResults o2) {
        return (int) (o1.getDocumentScore() - o2.getDocumentScore());
    }
}
