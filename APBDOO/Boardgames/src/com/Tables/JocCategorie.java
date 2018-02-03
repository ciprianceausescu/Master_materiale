package com.Tables;

public class JocCategorie {
	private long joc_categorie_id;
	private String nume;
	private String descriere;
	public long getJoc_categorie_id() {
		return joc_categorie_id;
	}
	public void setJoc_categorie_id(long joc_categorie_id) {
		this.joc_categorie_id = joc_categorie_id;
	}
	public String getNume() {
		return nume;
	}
	public void setNume(String nume) {
		this.nume = nume;
	}
	public String getDescriere() {
		return descriere;
	}
	public void setDescriere(String descriere) {
		this.descriere = descriere;
	}
}
