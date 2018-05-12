package main;



public class DocResults implements Comparable<DocResults>{
    private double score;
    private int docID;
    private String docName;

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

    public void setScore(double score) {
      this.score = score;
    }

    public int getDocID() {
        return docID;
    }

    public void setDocID(int docID) {
      this.docID = docID;
    }

    @Override
    public int compareTo(DocResults o) {
        if (this.score < o.score) return 1;
        if (this.score > o.score) return -1;
        return 0;
    }
    public boolean hasTerm(String word){
        return true;
    }
}
