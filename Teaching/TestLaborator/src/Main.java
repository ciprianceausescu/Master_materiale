import java.io.*;
import java.util.ArrayList;
import java.util.Comparator;

/**
 * Created by Ciprian Mihai on 5/18/2018.
 */
public class Main {
    public static void main(String[] args) throws IOException, ClassNotFoundException {
        ArrayList<Animal> list = new ArrayList<>();
        list.add(new Caine(10,20, "Azorel"));
        list.add(new Pisica(12, 25, "Miaunica"));

        for (Animal a:
             list ) {
            if(a instanceof Caine){
                Caine c = (Caine)a;
                c.latra();
            }
            if(a instanceof Pisica){
                Pisica p = (Pisica)a;
                p.miauna();
            }
        }

        ArrayList<Pisica> listPisici = new ArrayList<>();
        listPisici.add(new Pisica(10,15,"A"));
        listPisici.add(new Pisica(13,15,"B"));
        listPisici.add(new Pisica(7,15,"C"));
        listPisici.add(new Pisica(5,15,"D"));
        listPisici.add(new Pisica(17,15,"E"));

        listPisici.sort(new Comparator<Pisica>() {
            @Override
            public int compare(Pisica o1, Pisica o2) {
                return o1.getVarsta()-o2.getVarsta();
            }
        });
        for (Pisica p:
             listPisici) {
            System.out.println(p);
        }

        File f = new File("pisica.txt");
        FileOutputStream fos = new FileOutputStream(f);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(new Pisica(5, 4, "Cat"));

        Pisica p = null;

        FileInputStream fis = new FileInputStream(f);
        ObjectInputStream ois = new ObjectInputStream(fis);
        p = (Pisica)ois.readObject();
        System.out.println(p);
    }
}
