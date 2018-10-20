package lab3;

import java.util.Comparator;

/**
 * O clasa Circle. Dorim sa exemplificam obiectele imutabile
 * Observati cuvantul cheie "final".
 *
 * Amintim:
 *  - Variabilele declarate cu atributul final pot fi inițializate
 *    o singură dată, fie printr-un constructor, fie printr-o asignare.
 *    Dacă toate atributele unui obiect admit o unică inițializare,
 *    spunem că acel obiect este immutable , în sensul că starea lui
 *    internă nu se poate modifica.
 *  - Exemple de astfel de obiecte sunt instanțe ale claselor String
 *    și Integer.
 *  - Unei variabile de tip referință care are atributul final îi poate
 *    fi asignată o singură valoare (variabila poate puncta către un singur
 *    obiect). O încercare nouă de asignare a unei astfel de variabile va
 *    avea ca efect generarea unei erori la compilare. Totuși, obiectul către
 *    care punctează o astfel de variabilă poate fi modificat (prin apeluri
 *    de metodă sau acces la câmpuri).
 */
public class Circle {

    public static final double PI = 3.14;

    public final double radius;

    public final Position position;

    public Circle(double radius, Position position) {
        this.radius = radius;
        this.position = position;
    }

    public double getRadius() {
        return radius;
    }

    public Position getPosition() {
        return position;
    }

    @Override
    public String toString() {
        return "Circle{" +
                "radius=" + radius +
                ", position=" + position +
                '}';
    }

    public static void main(String[] args) {

        Circle circle = new Circle(2, new Position(1, 2));
        System.out.println(circle);

//      circle.radius = 3; // Eroare!
//      circle.position = new Position(2, 3); // Eroare
//
        circle.position.x = 2;
        circle.position.y = 7;

        System.out.println(circle);
    }
}
