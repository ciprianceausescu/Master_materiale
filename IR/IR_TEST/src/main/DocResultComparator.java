package main;

/**
 * Created by Ciprian Mihai on 5/01/2018.
 */

import java.util.Comparator;
//Comparator realizat implementând interfața Comparator, care sortează documentele în ordine descrescătoare a scorurilor
public class DocResultComparator implements Comparator<DocResults> {

    @Override
    public int compare(DocResults o1, DocResults o2) {
        //Dacă o1 are scor mai mic decât o2, return 1
        if(o1.getDocumentScore()<o2.getDocumentScore())
            return 1;
        //Dacă o1 are scor mai mare decât o2, return -1
        if(o1.getDocumentScore()>o2.getDocumentScore())
            return -1;
        //Dacă o1 are scor egal cu o2, return 0
        return 0;
    }
}
