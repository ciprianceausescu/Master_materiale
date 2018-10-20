package lab6_lab7.sort;

public class SortMain {
    public static void main(String aa[]) {

        int a[] = {5, 3, 1, 2}, na = a.length;
        float f[] = {1, 5, 2, 4};
        int nf = f.length;
        ISort i1 = new SortJava(f, nf); // tip interfata
        i1.sort();
        i1.afis();
        //i1.sum(); //-nu se poate
        SortJava s = (SortJava) i1;
        System.out.println(s.sum());
        i1 = new OtherSort(a, na); // alta implementare a interfetei
        i1.sort();
        i1.afis();
    }
}