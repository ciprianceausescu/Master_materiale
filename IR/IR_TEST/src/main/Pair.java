package main;

/**
 * Created by Ciprian Mihai on 5/01/2018.
 */

public class Pair{
    private char[] query;
    private int index;
    public Pair(char[] query, int index){
        this.query = query;
        this.index = index;
    }
    public char[] getQuery(){ return query; }
    public int getIndex(){ return index; }
    public void setQuery(char[] query){ this.query = query; }
    public void setIndex(int index){ this.index = index; }
}
