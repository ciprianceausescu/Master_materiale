package ex1;

import java.io.Serializable;

public abstract class Animal implements Serializable {

	private static final long serialVersionUID = -6859923604185046489L;

	public Animal() {

	}

	public Animal(Integer varsta, Double greutate, String nume) {
		super();
		this.varsta = varsta;
		this.greutate = greutate;
		this.nume = nume;
	}

	protected Integer varsta;

	protected Double greutate;

	protected String nume;

	public abstract String mananca();

	public Integer getVarsta() {
		return varsta;
	}

	public Double getGreutate() {
		return greutate;
	}

	public String getNume() {
		return nume;
	}

	public void setVarsta(Integer varsta) {
		this.varsta = varsta;
	}

	public void setGreutate(Double greutate) {
		this.greutate = greutate;
	}

	public void setNume(String nume) {
		this.nume = nume;
	}

}
