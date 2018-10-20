package lab4_lab5;

/**
 * Un exemplu simplu de mostenire.
 * Amintim ordinea de executare a constructorilor:
 *  - La crearea unui obiect de tipul subclasei se apelează
 *    constructori din ambele clase: mai întâi cel din superclasă
 *    și apoi cel din subclasă.
 *  - Dacă apelul supernu există, compilatorul va încerca să îl
 *    atașeze pe cel fără argumente, ceea ce va cauza o eroare dacă
 *    în clasa de bază nu este definit un constructor fără argumente.
 *  - Fiecare constructor din subclasă trebuie să aibă un constructor
 *    cu aceeași signatură în clasa părinte sau să apeleze explicit
 *    un alt constructor al clasei extinse.
 */
public class ClassTwo extends ClassOne {

    int y;

    public ClassTwo(int x, int y) {
        super(x);
        System.out.println("ClassTwo constructor");
        this.y = y;
    }

    public static void main(String[] args) {
        ClassTwo class2 = new ClassTwo(1, 3);
        System.out.println(class2.x);
        System.out.println(class2.y);
    }
}
