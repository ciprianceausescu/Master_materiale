package Proiect1;

/**
 * Created by Ciprian Mihai on 3/14/2017.
 */
public class verificareVector {
    public static boolean verificare(int n, int v[], int count){
        if (count<0 || count>n-1 || n<1 || n>50 || v.length!=n) {
            System.out.println("Conditii nesatisfacute");
            return false;
        }
        for (int i=0;i<v.length;i++)
            if (v[i]<0){
                System.out.println("Conditii nesatisfacute");
                return false;
            }
        return count == numarPerechi(v);
    }
    public static int numarPerechi(int v[]){
        int numar = 0;
        for (int i=0;i<v.length-1;i++)
        if (prime_intre_ele(v[i],v[i+1]))
            numar += 1;
        return numar;
    }
    public static boolean prime_intre_ele(int a, int b){
        return cmmdc(a, b) == 1;
    }
    public static int cmmdc(int a, int b){
        int c;
        while (b!=0) {
            c = a % b;
            a = b;
            b = c;
        }
        return a;
    }
}
