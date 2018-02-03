package com.Tables;

import com.DAO.Implementations.JocNrJucatoriImpl;
import com.DAO.Implementations.Librarie;

public class Joc {
	private int joc_id;
	private String nume;
	private String imagine;
	private String descriere;
	private long an_publicare;
	private long joc_categorie_id;
	private long joc_producator_id;
	private long joc_tip_id;
	private long joc_numar_jucatori_id;
	
	public int getJoc_id() {
		return joc_id;
	}
	public void setJoc_id(int joc_id) {
		this.joc_id = joc_id;
	}
	public String getNume() {
		return nume;
	}
	public void setNume(String nume) {
		this.nume = nume;
	}
	public String getImagine() {
		return imagine;
	}
	public void setImagine(String imagine) {
		this.imagine = imagine;
	}
	public String getDescriere() {
		return descriere;
	}
	public void setDescriere(String descriere) {
		this.descriere = descriere;
	}
	public long getAn_publicare() {
		return an_publicare;
	}
	public void setAn_publicare(long an_publicare) {
		this.an_publicare = an_publicare;
	}
	public long getJoc_categorie_id(){
		return joc_categorie_id;
	}
	public void setJoc_categorie_id(long joc_categorie_id) {
		this.joc_categorie_id = joc_categorie_id;
	}
	public long getJoc_producator_id() {
		return joc_producator_id;
	}
	public void setJoc_producator_id(long joc_producator_id) {
		this.joc_producator_id = joc_producator_id;
	}
	public long getJoc_tip_id() {
		return joc_tip_id;
	}
	public void setJoc_tip_id(long joc_tip_id) {
		this.joc_tip_id = joc_tip_id;
	}
	public long getJoc_numar_jucatori_id() {
		return joc_numar_jucatori_id;
	}
	public void setJoc_numar_jucatori_id(long joc_numar_jucatori_id) {
		this.joc_numar_jucatori_id = joc_numar_jucatori_id;
	}
	public JocCategorie getJoc_categorie() {
		return Librarie.getJocCategorieImpl().getGameCategory(this.getJoc_categorie_id());
	}
	public JocProducator getJoc_producator() {
		return Librarie.getJocProducatorImpl().getGameProducer(this.getJoc_producator_id());
	}
	public JocTip getJoc_tip() {
		return Librarie.getJocTipImpl().getGameType(this.getJoc_tip_id());
	}
	public JocNrJucatori getJocNrJucatori(){
		return Librarie.getJocNrJucatoriImpl().getNumberPlayers(this.getJoc_numar_jucatori_id());
	}
}
