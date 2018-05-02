
public class Automobil {
	String marca;
	int capacitate;
	int pret;
	public Automobil(String marca, int capacitate, int pret) {
		super();
		this.marca = marca;
		this.capacitate = capacitate;
		this.pret = pret;
	}
	@Override
	public String toString() {
		return "Automobil [marca=" + marca + ", capacitate=" + capacitate
				+ ", pret=" + pret + "]";
	}
	
}
