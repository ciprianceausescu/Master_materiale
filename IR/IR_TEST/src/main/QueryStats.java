package main;

public class QueryStats {
    String token;
    double idf;
    int df;
    int tfq;

    @Override
    public String toString() {
        return  "\""+ token + "\"" + "\t with Doc Freq = " + df +
            "\t | IDF = " + idf;
    }
}
