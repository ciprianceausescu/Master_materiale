package main;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;

public class DocResults {
        private double score;
        private int docID;
        private String docName;
        private List<TermFrequencies> termsFreq;

        public DocResults(){}

        public DocResults(double score, int docID, String docName) {
            this.score = score;
            this.docID = docID;
            this.docName = docName;
        }

        public String getDocName() {
            return docName;
        }

        public void setDocName(String docName) {
            this.docName = docName;
        }

        public double getScore() {

            return score;
        }

        @Override
        public String toString() {
            String docResultsString = docName + "\t score=" + score + "\t ";
            for (TermFrequencies termFrequencies : termsFreq) {
                docResultsString += termFrequencies.toString();
            }
            return docResultsString;
        }

        public void setScore(double score) {
            this.score = score;
        }

        public int getDocID() {   return docID;  }

        public void setDocID(int docID) {
            this.docID = docID;
        }

        public List<TermFrequencies> getTermsFreq() {
            return termsFreq;
        }

        public void setTermsFreq(List<TermFrequencies> newtermsFreq) {
            this.termsFreq = newtermsFreq;
        }

        boolean hasTerm(String term){
            for (TermFrequencies termFrequencies : termsFreq) {
                if(termFrequencies.term.equals(term)) return true;
            }
            return false;
        }
    }
