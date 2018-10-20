package lab4_lab5;

public class Persoana {

    String nume;
    int varsta;

    public Persoana(String nume) {
        this.nume = nume;
    }

    public Persoana(String nume, int varsta) {
        this.nume = nume;
        this.varsta = varsta;
    }

    @Override
    public boolean equals(Object o) {
        if(o == null) {
            return false;
        }
        if(o instanceof Persoana) {
            Persoana persoana = (Persoana) o;
            return varsta == persoana.varsta && nume.equals(persoana.nume);
        }
        return false;
    }

    @Override
    public int hashCode() {
        int result = 17;
        result = 31 * result + varsta;
        return result;
    }

    @Override
    public String toString() {
        return nume + ", " + varsta;
    }
}
