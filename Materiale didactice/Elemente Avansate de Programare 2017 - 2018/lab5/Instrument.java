package lab6_lab7;

/**
 * O interfata Intrument.
 * Amintim:
 * O interfață este o colecție de declarații de constante (câmpuri cu
 * modificatorii static și final) și metode abstracte (fără implementare),
 * stabilind o “formă” (schelet) pentru clasele care o implementează.
 * Intuitiv, interfețele determină un “contract”, un protocol între clase,
 * respectiv o clasă care implementează o interfață trebuie să implementeze
 * metodele definite în acea interfață.
 */
public interface Instrument {

    int CONSTANT_INT = 1; // Constanta; static & final

    void play(); // Automat public
    String what();
    void adjust();

    //protected int doSomething; // Modificator nepermis
    //private String doSomethingElse; // Modificator nepermis
}