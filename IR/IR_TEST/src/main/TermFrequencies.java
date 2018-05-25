package main;

/**
 * Created by Ciprian Mihai on 5/01/2018.
 */

import java.text.DecimalFormat;
import java.util.List;

public class TermFrequencies {
    String term;
    float freq;
    int totalWords;

    public TermFrequencies(String term, float freq) {
        this.term = term;
        this.freq = freq;
    }

    public int getTotalWords() {
        return totalWords;
    }

    public void setTotalWords(int totalWords) {
        this.totalWords = totalWords;
    }

    @Override
    public String toString() {
        DecimalFormat df = new DecimalFormat();
        df.setMaximumFractionDigits(2);
        return "TF: \"" + term + "\" = " + freq + "/" + totalWords + " = " +df.format(freq/totalWords) +" | ";
  }
}
