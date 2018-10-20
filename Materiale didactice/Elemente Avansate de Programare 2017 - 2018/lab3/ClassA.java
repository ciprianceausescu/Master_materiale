package lab3;

/**
 * O clasa prin care exemplificam metode ale clase Object.
 * Are doua cmpuri: doua numere intregi, x si y.
 *
 * Amintim:
 *  - Metoda equals verifică dacă un obiect este egal (în
 *  relație de echivalență) cu cel dat ca referință. Foarte
 *  important de reținut este faptul că operatorul de egalitate
 *  “==” verifică egalitatea pe referințe, deci ob1 == ob2 dacă
 *  și numai dacă cele două obiecte reprezintă aceeași referință.
 *  În practică, cel mai adesea se dorește verificarea egalității
 *  câmpurilor obiectelor. În aceast caz, suprascrierea metodei
 *  equals este necesară.
 */
public class ClassA {

    int x;
    int y;

    public ClassA(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof ClassA && this.x == ((ClassA) obj).x && this.y == ((ClassA) obj).y;
    }

    @Override
    public String toString() {
        return "(" + x + ", " + y + ")";
    }

    @Override
    public int hashCode() {
        int hash = 1;
        hash = hash * 31 + x;
        hash = hash * 17 + y;
        return hash;
    }

    public static void main(String[] args) {

        ClassA ob1 = new ClassA(2, 3);
        ClassA ob2 = ob1; // Aceeași referință


        System.out.println(ob1 == ob2);

        ClassA ob3 = new ClassA(2, 3);
        System.out.println(ob1 == ob3); // Nu e aceeași referință

        System.out.println(ob1.equals(ob2));
        System.out.println(ob1.equals(ob3));

        ob2.x = 5;
        ob3.x = 7;

        System.out.println("Ob1: (x,y)=(" + ob1.x + ',' + ob1.y + ");");
        System.out.println("Ob2: (x,y)=(" + ob2.x + ',' + ob2.y + ");");
        System.out.println("Ob3: (x,y)=(" + ob3.x + ',' + ob3.y + ");");
    }

}
