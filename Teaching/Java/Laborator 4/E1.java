public class E1 {
    public static void main(String[] args) {
        Profesor p = new Profesor("David", "ASD");
        Student s = new Student("David", 10);
        System.out.println(p.toString());
        System.out.println(s.toString());
        //5
        Persoana vp[] = new Persoana[3];
        vp[0] = new Profesor("Nume1", "materie1");
        vp[1] = new Student("nume2", 2, "198789876654");
        vp[2] = new Profesor("Nume3", "materie3");
        for (Persoana i:vp)
            System.out.println(i.toString());
        //6
        for (Persoana i:vp) {
            //((Profesor)i).preda();
            if (i instanceof Profesor)
                ((Profesor)i).preda();
            else if (i instanceof Student)
                ((Student)i).invata();
        }

    }

}

class Persoana {
    String nume;

    public Persoana(String nume) {
        this.nume = nume;
    }

    public Persoana() {
        this.nume = "default";
    }

    @Override
    public String toString() {
        return nume;
    }
}

class Profesor extends Persoana {
    String materie;

    public Profesor(String nume, String materie) {
        super(nume);
        this.materie = materie;
    }

    public Profesor() {
        super();
    }

    @Override
    public String toString() {
        //return super.toString();
        return "Profesor: " + nume + ", " + materie;
    }

    void preda() {
        System.out.println("Preda " + nume);
    }
}

class Student extends Persoana {
    int nota;
    String cnp;

    public Student(String nume, int nota, String cnp) {
        super(nume);
        this.nota = nota;
        this.cnp= cnp;
    }

    public Student(String nume, int nota) {
        super(nume);
        this.nota = nota;
    }

    public Student() {
        super();
        nota = -1;
    }

    @Override
    public String toString() {
        //return super.toString();
        return "Student: " + nume + ", " + nota;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (obj == this)
            return true;
        if (!(obj instanceof Student))
            return false;
        Student t = (Student)(obj);
        return this.cnp.equals(t.cnp);
    }

    void invata() {
        System.out.println("Invata " + nume);
    }
}