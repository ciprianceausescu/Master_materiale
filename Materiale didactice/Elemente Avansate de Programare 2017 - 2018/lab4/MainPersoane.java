package lab4_lab5;

public class MainPersoane {

    public static void main(String[] args) {

        Persoana[] persoane = new Persoana[2];
        persoane[0] = new Profesor("Gigel", 80);
        persoane[1] = new Student(8, "Ion", 18);

        System.out.println(persoane[0]);
        System.out.println(persoane[1]);
    }
}
