package main;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.text.DecimalFormat;

@Getter
@AllArgsConstructor
class QueryResult implements Comparable<QueryResult>{
    private String path;
    private double score;
    private double tf;

    @Override
    public String toString(){
        DecimalFormat df = new DecimalFormat();
        df.setMaximumFractionDigits(2);
        return "Doc: " + this.path + ". Score: " + df.format(score) + ". TF: " + df.format(tf);
    }

    @Override
    //Am implementat Comparable pentru a sorta rezultatele Query-ului in ordine descendenta
    public int compareTo(QueryResult o) {
        if (this.score < o.score) return 1;
        if (this.score > o.score) return -1;
        return 0;
    }
}
