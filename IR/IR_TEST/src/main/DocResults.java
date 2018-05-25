package main;

/**
 * Created by Ciprian Mihai on 4/01/2018.
 */

import java.util.List;

public class DocResults {
    private double documentScore;
    private int documentUniquiIdentifier;
    private String documentName;
    private List<TermFrequencies> termFrequenciesSimpleList;
    private int occurenceOfWord;

    public String getDocumentName() {
    return documentName;
  }

    public void setDocumentName(String documentName) {
    this.documentName = documentName;
  }

    public double getDocumentScore() { return this.documentScore; }

    @Override
    public String toString() {
        String docResultsString = "\"" + documentName + "\" | " + "\t documentScore = " + documentScore + "\t | ";
        for (TermFrequencies termFrequencies : termFrequenciesSimpleList) {
            termFrequencies.setTotalWords(occurenceOfWord);
            docResultsString += termFrequencies.toString();
        }
        return docResultsString;
    }

    public void setDocumentScore(double documentScore) {
    this.documentScore = documentScore;
  }

    public int getDocumentUniquiIdentifier() { return documentUniquiIdentifier; }

    public void setDocumentUniquiIdentifier(int documentUniquiIdentifier) {
    this.documentUniquiIdentifier = documentUniquiIdentifier;
  }

    public List<TermFrequencies> getTermFrequenciesSimpleList() {
    return termFrequenciesSimpleList;
  }

    public void setTermFrequenciesSimpleList(List<TermFrequencies> newtermsFreq) {
    this.termFrequenciesSimpleList = newtermsFreq;
  }

    boolean hasTerm(String term){
        for (TermFrequencies termFrequencies : termFrequenciesSimpleList) {
            if(termFrequencies.term.equals(term)) return true;
        }
        return false;
    }

    public int getOccurenceOfWord() {
        return occurenceOfWord;
    }

    public void setOccurenceOfWord(int occurenceOfWord) {
        this.occurenceOfWord = occurenceOfWord;
    }
}