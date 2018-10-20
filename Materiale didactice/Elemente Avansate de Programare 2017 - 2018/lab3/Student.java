package lab3;

/**
 * O clasa Student care encapsuleaza date despre studentii unei
 * facultati.
 *
 * Amintim:
 *  - Cuvântul cheie "this" se referă la instanța curentă a clasei
 *  și poate fi folosit de metodele (care nu sunt statice!) ale unei
 *  clase pentru a referi obiectul curent. Poate fi folosit de exemplu
 *  pentru a face diferența dintre câmpuri ale obiectului curent și
 *  argumentele care au același nume.
 */
public class Student {

    /**
     * Numele studentului
     */
    private String name;

    /**
     * Media generala
     */
    private double averageGrade;

    /**
     * Un identificator pentru un student
     */
    private int id;

    /**
     * O variabila statica ce pastreaza numarul
     * de ordine al unui student.
     */
    private static int num;

    /**
     * Constructor
     * @param name Numele studentului
     * @param averageGrade Media generala
     */
    public Student(String name, double averageGrade) {
        this.name = name;
        this.averageGrade = averageGrade;
        this.id = ++num;
    }

    /**
     * Constructor
     * @param name Numele studentului
     */
    public Student(String name) {
        this(name, 0.0);
    }

    public void setAverageGrade(double averageGrade) {
        this.averageGrade = averageGrade;
    }

    /**
     * Intoarce id-ul studentului
     * @return id
     */
    public int getId() {
        return this.id;
    }

    @Override
    public String toString() {
        return "Student{" +
                "name='" + name + '\'' +
                ", averageGrade=" + averageGrade +
                ", id=" + id +
                '}';
    }

    public static void main(String[] args) {

        Student s1 = new Student("Ion", 10);
        Student s2 = new Student("Gigi", 5.4);
        Student s3 = new Student("Ana");
        s3.setAverageGrade(8.80);

        /* Amintim ca atunci cand se printeaza un obiect,
           se cheama metoda toString(), pe care am suprascris-o
           mai sus, pentru a printa toate detaliile despre
           un student.
         */
        System.out.println(s1);
        System.out.println(s2);
        System.out.println(s3);

    }


}
