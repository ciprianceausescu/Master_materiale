package main;

/**
 * Created by Ciprian Mihai on 5/01/2018.
 */

public class QueryStatuses {
    String tokenElement;
    double idf;
    int df;
    int tfq;

    @Override
    public String toString() {
        return  "\""+ tokenElement + "\"" + "\t with Doc Freq = " + df +
            "\t | IDF = " + idf;
    }
}
