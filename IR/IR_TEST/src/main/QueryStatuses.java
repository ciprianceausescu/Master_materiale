package main;

/**
 * Created by Ciprian Mihai on 5/01/2018.
 */
//Clasă care oferă informații despre o anumită interogare, și anume elementul care face parte din interogare, alături de
//IDF, DF și TF
public class QueryStatuses {
    String tokenElement;
    double idf;
    int df;
    int tfq;
    //Metoda care afișează informațiile despre interogare
    @Override
    public String toString() {
        return  "\""+ tokenElement + "\"" + "\t with Doc Freq = " + df +
            "\t | IDF = " + idf;
    }
}
