package com.DAO.Implementations;

public class Librarie {
	public static JocImpl getJocImpl(){
		return new JocImpl();
	}
	public static AngajatImpl getAngajatImpl(){
		return new AngajatImpl();
	}
	public static JocCategorieImpl getJocCategorieImpl(){
		return new JocCategorieImpl();
	}
	public static JocProducatorImpl getJocProducatorImpl(){
		return new JocProducatorImpl();
	}
	public static JocTipImpl getJocTipImpl(){
		return new JocTipImpl();
	}
	public static JocNrJucatoriImpl getJocNrJucatoriImpl(){
		return new JocNrJucatoriImpl();
	}
	public static AdresaImpl getAdresaImpl(){
		return new AdresaImpl();
	}
}
