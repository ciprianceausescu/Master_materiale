package lab4_lab5;

public class Student extends Persoana {

    private static final int NOTA_IMPLICITA = 4;
    private int nota;

    public Student(int nota, String nume, int varsta) {
        super(nume, varsta);
        this.nota = nota;
    }


    public String toString() {
        return "Student: " + super.toString() + ", " + nota;
    }

    public void invata() {
        System.out.println("Studentul " + nume + " invata");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        Student student = (Student) o;
        return nota == student.nota && nume.equals(student.nume);
    }


}
