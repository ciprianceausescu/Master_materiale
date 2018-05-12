package main;
import java.util.List;

public class TermFrequencies {
  String term;
  float freq;

  public TermFrequencies(String term, float freq) {
    this.term = term;
    this.freq = freq;
  }

  @Override
  public String toString() {
    return term + "="+ freq +" ";
  }
}
