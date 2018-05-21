package src;

import java.util.*;

public class Persoana {
    private String nume;
    private String prenume;
    private int varsta;

    @Override
    public String toString() {
        return "src.Persoana{" +
                "nume='" + nume + '\'' +
                ", prenume='" + prenume + '\'' +
                ", varsta=" + varsta +
                '}';
    }

    public Persoana(String nume, String prenume, int varsta) {
        this.nume = nume;
        this.prenume = prenume;
        this.varsta = varsta;
    }

    public String getNume() {
        return nume;
    }

    public void setNume(String nume) {
        this.nume = nume;
    }

    public String getPrenume() {
        return prenume;
    }

    public void setPrenume(String prenume) {
        this.prenume = prenume;
    }

    public int getVarsta() {
        return varsta;
    }

    public void setVarsta(int varsta) {
        this.varsta = varsta;
    }
}

class sortareNume implements Comparator<Persoana> {
    @Override
    public int compare(Persoana o1, Persoana o2) {
        return o1.getNume().compareTo(o2.getNume());
    }
}

class sortarePrenume implements Comparator<Persoana> {
    @Override
    public int compare(Persoana o1, Persoana o2) {
        return o1.getNume().compareTo(o2.getNume());
    }
}

class sortareVarsta implements Comparator<Persoana> {
    @Override
    public int compare(Persoana o1, Persoana o2) {
        return o1.getVarsta()-o2.getVarsta();
    }
}



class Test{
    public static void main(String[] args) {
        ArrayList<Persoana> list = new ArrayList<Persoana>();
        list.add(new Persoana("Popescu", "Ion", 20));
        list.add(new Persoana("Popescu", "Maria", 22));
        list.add(new Persoana("Ionescu", "Marcel", 23));
        list.add(new Persoana("Ionescu", "Marcel", 23));
        list.add(new Persoana("Marin", "Anca", 21));
        list.add(new Persoana("Albu", "Ionel", 20));

        HashMap<String, Integer> hm = new HashMap<>();
        for(Persoana p:list)
            hm.put(p.getNume(), 1);

        for (Map.Entry m:hm.entrySet()){
            System.out.println("Persoanele cu numele: " + m.getKey());
            for(Persoana p:list)
                if(p.getNume().equals(m.getValue()))
                    System.out.println(p);
        }


    }
}
