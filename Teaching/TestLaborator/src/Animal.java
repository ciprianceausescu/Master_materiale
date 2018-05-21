import java.io.Serializable;

/**
 * Created by Ciprian Mihai on 5/18/2018.
 */
public abstract  class Animal implements Serializable{
    private int varsta;
    private int greutate;
    private String nume;

    public Animal(int varsta, int greutate, String nume) {
        this.varsta = varsta;
        this.greutate = greutate;
        this.nume = nume;
    }

    public int getVarsta() {
        return varsta;
    }

    public void setVarsta(int varsta) {
        this.varsta = varsta;
    }

    public int getGreutate() {
        return greutate;
    }

    public void setGreutate(int greutate) {
        this.greutate = greutate;
    }

    public String getNume() {
        return nume;
    }

    public void setNume(String nume) {
        this.nume = nume;
    }

    @Override
    public String toString() {
        return "Animal{" +
                "varsta=" + varsta +
                ", greutate=" + greutate +
                ", nume='" + nume + '\'' +
                '}';
    }

    abstract void mananca();
}
