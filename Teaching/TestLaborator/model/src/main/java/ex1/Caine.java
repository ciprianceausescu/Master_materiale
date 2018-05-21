package ex1;

public class Caine extends Animal {

	private static final long serialVersionUID = -1613167000635308538L;

	public Caine(Integer varsta, Double greutate, String nume) {
		super(varsta, greutate, nume);
	}

	@Override
	public String mananca() {
		return "oase";
	}

	public void latra() {
		System.out.println("cainele latra");
	}

}
