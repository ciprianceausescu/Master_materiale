package lab3;

/**
 * Amintim:
 * - Atunci când se creează mai multe instanțe ale aceleiași clase,
 *   fiecare dintre acestea au propriile copii ale variabilelor membre.
 *   Modificarea stării unei instanțe nu influențează starea altei
 *   instanțe, fiecare entitate fiind independentă una de cealaltă.
 * - Există, totuși, posibilitatea ca uneori, anumite câmpuri din cadrul
 *   unei clase să aibă valori independente de instanțele acelei clase,
 *   fiind comune tuturor instanțelor. Aceste câmpuri se declară folosind
 *   cuvântul cheie "static" și le vom denumi câmpuri statice sau variabile
 *   de clasă. Ele sunt asociate cu clasa respectivă și nu cu un obiect anume.
 */
public class StaticFieldExample {

    static String staticField = "Static field in class";

    private static String privateStaticField = "Private static field in class";

    public static String getPrivateStaticField() {
        return privateStaticField;
    }

}

class StaticFieldTest {

    public static void main(String[] args) {

        System.out.println(StaticFieldExample.staticField);

        /* Câmpul static privat privateStaticField nu poate
            fi accesat direct, ci numai prin getter. */
        System.out.println(StaticFieldExample.getPrivateStaticField());

    }

}