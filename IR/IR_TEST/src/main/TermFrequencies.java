package main;

/**
 * Created by Ciprian Mihai on 5/01/2018.
 */

import java.text.DecimalFormat;
import java.util.List;
//Clasă care oferă informații despre frecvența cuvintelor din interogare
public class TermFrequencies {
    //Element al interogării
    String term;
    //Frecvența cuvântului
    float freq;
    //Numărul total de apariții
    int totalWords;
    //Constructorul clasei
    public TermFrequencies(String term, float freq) {
        this.term = term;
        this.freq = freq;
    }
    //Metodă care returnează numărul total de cuvinte determinate
    public int getTotalWords() {
        return totalWords;
    }
    //Metodă care schimbă numărul total de cuvinte determinate
    public void setTotalWords(int totalWords) {
        this.totalWords = totalWords;
    }
    //Metodă care oferă o reprezentare sub formă de String a informațiilor despre cuvintele din interogare
    @Override
    public String toString() {
        DecimalFormat df = new DecimalFormat();
        df.setMaximumFractionDigits(2);
        return "TF: \"" + term + "\" = " + freq + "/" + totalWords + " = " +df.format(freq/totalWords) +" | ";
  }
}
