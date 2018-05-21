import com.sun.xml.internal.ws.developer.Serialization;

import java.io.*;
import java.util.*;

public class Persoana implements Serializable{
    private String nume;
    private String prenume;
    private int varsta;

    @Override
    public String toString() {
        return "Persoana{" +
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

class SortareNume implements Comparator<Persoana> {
    @Override
    public int compare(Persoana o1, Persoana o2) {
        return o1.getNume().compareTo(o2.getNume());
    }
}

class SortarePrenume implements Comparator<Persoana> {
    @Override
    public int compare(Persoana o1, Persoana o2) {
        return o1.getPrenume().compareTo(o2.getPrenume());
    }
}

class SortareVarsta implements Comparator<Persoana> {
    @Override
    public int compare(Persoana o1, Persoana o2) {
        return o1.getVarsta()-o2.getVarsta();
    }
}



class Test{
    public static void main(String[] args) {
        //Sortare in functie de diverse clase Comparator
        /*ArrayList<Persoana> list = new ArrayList<Persoana>();
        list.add(new Persoana("Popescu", "Ion", 20));
        list.add(new Persoana("Popescu", "Maria", 22));
        list.add(new Persoana("Ionescu", "Marcel", 23));
        list.add(new Persoana("Marin", "Anca", 21));
        list.add(new Persoana("Albu", "Ionel", 20));

        System.out.println("Sortare nume");
        Collections.sort(list, new SortareNume());
        for(Persoana p:list)
            System.out.println(p);

        System.out.println("Sortare prenume");
        Collections.sort(list, new SortarePrenume());
        for(Persoana p:list)
            System.out.println(p);

        System.out.println("Sortare varsta");
        Collections.sort(list, new SortareVarsta());
        for(Persoana p:list)
            System.out.println(p);

        //HashMap cu frecventa varstelor din Lista
        Map<Integer, Integer> hm = new HashMap<>();
        for(Persoana p:list)
            if(hm.get(p.getVarsta())==null)
                hm.put(p.getVarsta(), 1);
            else
                hm.put(p.getVarsta(), hm.get(p.getVarsta()) + 1);

        for (Map.Entry m:hm.entrySet())
            System.out.println(m.getKey() + " " + m.getValue());*/

        //Citire din fisier. Frecventa cuvintelor
        ArrayList<Persoana> listFile = new ArrayList<Persoana>();
        ArrayList<Persoana> listDeser = new ArrayList<Persoana>();
        File file = new File("in2.txt");
        File outFile = new File("out.txt");
        File dataFile = new File("date.txt");
        BufferedReader br = null;
        BufferedReader brData = null;
        BufferedWriter bw = null;
        try {
            br = new BufferedReader(new FileReader(file));
            brData = new BufferedReader(new FileReader(dataFile));
            bw = new BufferedWriter(new FileWriter(outFile));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        Map<String, Integer> hm2 = new HashMap<>();
        String st;
        try {
            while ((st = br.readLine()) != null)
            {
                StringTokenizer st2 = new StringTokenizer(st, " ");
                while (st2.hasMoreTokens()){
                    String element = st2.nextToken();
                    if(hm2.get(element)==null)
                        hm2.put(element, 1);
                    else
                        hm2.put(element, hm2.get(element) + 1);
                }
            }
            for (Map.Entry m:hm2.entrySet())
            {
                bw.write(m.getKey() + " " + m.getValue() + "\n");
            }

            while ((st = brData.readLine()) != null)
            {
                StringTokenizer st2 = new StringTokenizer(st, " ");
                while (st2.hasMoreTokens()){
                    Persoana persoana = new Persoana(st2.nextToken(), st2.nextToken(), Integer.parseInt(st2.nextToken()));
                    listFile.add(persoana);
                }
            }
            br.close();
            bw.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        Collections.sort(listFile, new SortareVarsta());
        for(Persoana p:listFile){
            System.out.println(p);
        }

        File outSer = new File("f.ser");
        try {
            //Ser
            FileOutputStream fos = new FileOutputStream(outSer);
            ObjectOutputStream oos = new ObjectOutputStream(fos);
            oos.writeObject(listFile.get(0));
            oos.writeObject(listFile.get(1));

            //Deser
            FileInputStream fis = new FileInputStream(outSer);
            ObjectInputStream ois = new ObjectInputStream(fis);

            Persoana p = (Persoana)ois.readObject();
            System.out.println(p);


        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }
}


