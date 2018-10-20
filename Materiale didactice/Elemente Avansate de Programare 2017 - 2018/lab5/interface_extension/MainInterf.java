package lab6_lab7.interface_extension;

public class MainInterf {

    public static void met() {
        System.out.println("abc");
    }

    public static void main(String[] s) {
        B ob1 = new B();
        ob1.met();
        D ob2 = new D();
        ob2.met();
        ob2.otherMet();
        int c = -88;
        met();
        System.out.println(c + " " + IA.c + " " + IB.c + " " + IC.c + " " + ID.c + " " + B.c + " " + D.c);

    }
}
