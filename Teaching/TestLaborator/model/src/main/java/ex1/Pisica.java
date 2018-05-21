package ex1;

public class Pisica extends Animal implements Comparable<Pisica> {

	private static final long serialVersionUID = 1L;

	public Pisica() {

	}

	public Pisica(Integer varsta, Double greutate, String nume) {
		super(varsta, greutate, nume);
	}

	@Override
	public String mananca() {
		return "lapte";
	}

	public void miauna() {
		System.out.println("Pisica miauna");
	}

	@Override
	public String toString() {
		return "Pisica [varsta=" + varsta + ", greutate=" + greutate + ", nume=" + nume + "]";
	}

	@Override
	public int compareTo(Pisica o) {
		return this.getVarsta().compareTo(o.getVarsta());
	}
}
