package lab4_lab5;

public class Profesor extends Persoana {

    private static final String MATERIE_IMPLICITA = "Info";

    private String materie;


    public Profesor(String nume, int varsta) {
        super(nume, varsta);
        this.materie = MATERIE_IMPLICITA;
    }

    public Profesor(String nume, int varsta, String materie) {
        super(nume, varsta);
        this.materie = materie;
    }

    public void preda() {
        System.out.println("Profesorul" + nume + " preda ");
    }

    @Override
     public String toString() {
        return "Profesor: " + nume + ", " + materie;
    }
}
