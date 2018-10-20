package lab3;

public class DreptunghiMain {
    public static void main(String arg[]) {
        double a;

        //Cream un dreptunghi
        Dreptunghi d = new Dreptunghi(3, 5);
        System.out.print("Dreptunghi initial: ");
        d.afisare();
        a = d.arie();
        System.out.println("Arie = " + a);

        //cream un nou dreptunghi, de dimensiuni primite ca argumente
        int x = Integer.parseInt(arg[0]);
        int y = Integer.parseInt(arg[1]);
        Dreptunghi d2 = new Dreptunghi(x, y);
        System.out.print("Noul dreptunghi: ");
        d2.afisare();
        System.out.println("Arie = " + d2.arie());
        if (d.maiMare(d2))
            System.out.println("Noul dreptunghi este mai mare");
        else
            System.out.println("Dreptunghiul initial este mai mare");
        d2 = new Dreptunghi(); //se va apela constructorul fara argumente
        System.out.print("Dreptunghi unitate: ");
        d2.afisare();
    }
}

