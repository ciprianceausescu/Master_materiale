package lab3;

/**
 * Encapsuleaza date despre un punct in plan, ce determina o
 * pozitie.
 */
public class Position {

    double x;
    double y;

    public Position(double x, double y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public String toString() {
        return "(" + x + ", " + y + ")";
    }
}
