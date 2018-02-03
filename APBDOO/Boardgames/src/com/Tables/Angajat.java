package com.Tables;

import com.DAO.Implementations.AdresaImpl;
import com.DAO.Implementations.Librarie;

public class Angajat {
	private int angajat_id;
	private String nume;
	private long cnp;
	private long adresa;
	private long telefon;
	private int magazin_departament_id;
	private int angajat_contract_id;
	private int angajat_pozitie_id;
	
	public int getAngajat_id() {
		return angajat_id;
	}
	public void setAngajat_id(int angajat_id) {
		this.angajat_id = angajat_id;
	}
	public String getNume() {
		return nume;
	}
	public void setNume(String nume) {
		this.nume = nume;
	}
	public long getCNP() {
		return cnp;
	}
	public void setCNP(long cnp) {
		this.cnp = cnp;
	}
	public long getAdresa() {
		return adresa;
	}
	public void setAdresa(long adresa) {
		this.adresa = adresa;
	}
	public long getTelefon() {
		return telefon;
	}
	public void setTelefon(long telefon) {
		this.telefon = telefon;
	}
	public int getMagazin_departament_id() {
		return magazin_departament_id;
	}
	public void setMagazin_departament_id(int magazin_departament_id) {
		this.magazin_departament_id = magazin_departament_id;
	}
	public int getAngajat_contract_id() {
		return angajat_contract_id;
	}
	public void setAngajat_contract_id(int angajat_contract_id) {
		this.angajat_contract_id = angajat_contract_id;
	}
	public int getAngajat_pozitie_id() {
		return angajat_pozitie_id;
	}
	public void setAngajat_pozitie_id(int angajat_pozitie_id) {
		this.angajat_pozitie_id = angajat_pozitie_id;
	}
	public Adresa getAdresaInfo(){
		return Librarie.getAdresaImpl().getAddress(this.adresa);
	}
}
