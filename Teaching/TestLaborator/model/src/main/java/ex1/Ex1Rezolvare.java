package ex1;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

public class Ex1Rezolvare {
	public static void main(String[] args) {
		// b) Creati o lista in care sa stocati 2,3 pisici si 2, 3 caini
		ArrayList<Animal> animale = new ArrayList<Animal>();
		animale.add(new Pisica(2, 6.5D, "Pisica1"));
		animale.add(new Caine(12, 12D, "Caine1"));
		animale.add(new Pisica(4, 6.0D, "Pisica3"));
		animale.add(new Pisica(64, 0.2D, "Pisica2"));
		animale.add(new Caine(25, 32.0D, "Caine2"));
		// c) Parcurgeti lista create la pasul anterior si pentru fiecare element al
		// listei, apelati metoda “latra” sau “miauana” in functie de elemental curent
		for (Animal animal : animale) {
			if (animal instanceof Pisica) {
				Pisica pisica = (Pisica) animal;
				pisica.miauna();
			} else {
				if (animal instanceof Caine) {
					Caine caine = (Caine) animal;
					caine.latra();
				}
			}
		}
		// d) Creati o lista/vector de Pisici, adaugati 4,5 pisici si sortati lista in
		// functie de varsta pisici.
		ArrayList<Pisica> pisici = new ArrayList<>();

		pisici.add(new Pisica(64, 0.2D, "Pisica2"));
		pisici.add(new Pisica(2, 6.5D, "Pisica1"));
		pisici.add(new Pisica(4, 6.0D, "Pisica3"));
		Collections.sort(pisici);
		for (Pisica pisica : pisici) {
			System.out.println(pisica);
		}
		// alta alternativa pt d cu comparator
		Collections.sort(pisici, new Comparator<Pisica>() {

			@Override
			public int compare(Pisica o1, Pisica o2) {

				return o1.getVarsta().compareTo(o2.getVarsta());
			}
		});
		for (Pisica pisica : pisici) {
			System.out.println(pisica);
		}
		// e) a) Serializati in fisierul “pisica.txt” un obiect de tip pisica cu
		// urmatoarele attribute
		FileOutputStream fos = null;
		ObjectOutputStream oos = null;
		try {
			fos = new FileOutputStream("pisica.txt");
			oos = new ObjectOutputStream(fos);
			Pisica pisica = new Pisica(5, 3.4d, "Cat");
			oos.writeObject(pisica);
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (fos != null) {
				try {
					fos.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			if (oos != null) {
				try {
					oos.close();
				} catch (IOException e) {

					e.printStackTrace();
				}
			}
		}
		// b. Deserializati pisica scrisa in fisierul “pisica.txt” de la punctul
		// anterior si afisati atributele obiectului la consola.
		FileInputStream fis = null;
		ObjectInputStream ois = null;
		try {
			fis = new FileInputStream("pisica.txt");
			ois = new ObjectInputStream(fis);
			Object ob = ois.readObject();
			if (ob instanceof Pisica) {
				Pisica pisica = (Pisica) ob;
				System.out.println(pisica);
			}
		} catch (IOException e) {
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		} finally {
			if (fis != null) {
				try {
					fis.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			if (ois != null) {
				try {
					ois.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}

	}
}
