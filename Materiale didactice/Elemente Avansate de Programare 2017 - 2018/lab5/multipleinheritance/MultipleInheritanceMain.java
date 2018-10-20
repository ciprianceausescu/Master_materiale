package lab6_lab7.multipleinheritance;

public class MultipleInheritanceMain {

    public static void main(String[] args) {

        X ob1 = new CX1();
        X ob2 = new CX2();

        C obC = new C(ob1, ob2);
        /**
         * Nu este necesar ca C să ştie care este clasa ce implementează pe I1,
         * ci poate afla acest lucru prin intermediul unui constructor;
         * cu alte cuvinte, la crearea unei instanţieri a lui C, putem preciza ce implementare
         * a lui X să folosească.
         */
        obC.met1();
        System.out.print(" " + obC.met2());
    }
}
