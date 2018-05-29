package main;

/**
 * Created by Ciprian Mihai on 4/01/2018.
 */

import java.util.List;
//Folosind clasa DocResults se afișează informații utile despre rezultatele obținute în urma
//interogărilor realizate pe documentele indexate
public class DocResults {
    //Scorul documentului
    private double documentScore;
    //Identificatorul unic al documentului
    private int documentUniquiIdentifier;
    //Numele documentului
    private String documentName;
    //O listă de obiecte TermFrequencies care identifică frecvența termenilor interogării
    private List<TermFrequencies> termFrequenciesSimpleList;
    //Numărul de apariții ale unui anumit cuvânt
    private int occurenceOfWord;
    //Metodă care returnează numele documentului
    public String getDocumentName() {
    return documentName;
  }
    //Metodă care schimbă numele documentului
    public void setDocumentName(String documentName) {
    this.documentName = documentName;
  }
    //Metodă care returnează scorul documentului
    public double getDocumentScore() { return this.documentScore; }
    //Metodă care returnează o reprezentare clară a informațiilor despre un anumit rezultat obținut printr-o
    //interogare
    @Override
    public String toString() {
        String docResultsString = "\"" + documentName + "\" | " + "\t documentScore = " + documentScore + "\t | ";
        for (TermFrequencies termFrequencies : termFrequenciesSimpleList) {
            termFrequencies.setTotalWords(occurenceOfWord);
            docResultsString += termFrequencies.toString();
        }
        return docResultsString;
    }
    //Metodă care schimbă scorul documentului
    public void setDocumentScore(double documentScore) {
    this.documentScore = documentScore;
  }
    //Metodă care returnează identificatorul unic al documentului
    public int getDocumentUniquiIdentifier() { return documentUniquiIdentifier; }
    //Metodă care schimbă identificatorul unic al documentului
    public void setDocumentUniquiIdentifier(int documentUniquiIdentifier) {
        this.documentUniquiIdentifier = documentUniquiIdentifier;
    }
    //Metodă care returnează lista de apariții al unui rezultat
    public List<TermFrequencies> getTermFrequenciesSimpleList() {
    return termFrequenciesSimpleList;
  }
    //Metodă care schimbă lista de apariții al unui rezultat
    public void setTermFrequenciesSimpleList(List<TermFrequencies> newtermsFreq) {
        this.termFrequenciesSimpleList = newtermsFreq;
    }
    //Metodă care verifică dacă lista de apariții conține un anumit rezultat
    boolean hasTerm(String term){
        for (TermFrequencies termFrequencies : termFrequenciesSimpleList) {
            if(termFrequencies.term.equals(term)) return true;
        }
        return false;
    }
    //Metodă care returnează numărul de apariții ale unui cuvânt
    public int getOccurenceOfWord() {
        return occurenceOfWord;
    }
    //Metodă care schimbă numărul de apariții ale unui cuvânt
    public void setOccurenceOfWord(int occurenceOfWord) {
        this.occurenceOfWord = occurenceOfWord;
    }
}