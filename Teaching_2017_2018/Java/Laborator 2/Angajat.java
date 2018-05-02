package pkg1;

import java.util.Arrays;

public class Angajat {

    public Angajat() {
     n++;   
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize(); //To change body of generated methods, choose Tools | Templates.
        n--;
        System.out.println(this.toString() + " distrus");
    }

    @Override
    public String toString() {
        return "Angajat{" + "nume=" + nume + ", varsta=" + varsta + ", salariu=" + salariu + ", pozitie=" + pozitie + '}';
    }

    public Angajat(String nume, int varsta, float salariu) {
        this.nume = nume;
        this.varsta = varsta;
        this.salariu = salariu;
        pozitie = "fulltime";
        if (varsta < 25)
            pozitie = "intern";
        n++;
    }
    String nume;
    int varsta;
    static String firma = "BestIT";
    float salariu;
    String pozitie;
    static int n = 0;
    
    public static void main(String[] args) {        
        Angajat[] v = new Angajat[500000];
        for (int i = 0; i < v.length; i++)
            v[i] = new Angajat();
        System.out.println(Angajat.n);
         v = null;
         
          Angajat[] v2 = new Angajat[500000];
        for (int i = 0; i < v2.length; i++)
            v2[i] = new Angajat();
        System.out.println(Angajat.n);
         v2 = null;
         
          Angajat[] v3 = new Angajat[500000];
        for (int i = 0; i < v3.length; i++)
            v3[i] = new Angajat();
        System.out.println(Angajat.n);
         v3 = null;
         
         System.gc();
         
         System.out.println(Angajat.n);
    }
    
}
