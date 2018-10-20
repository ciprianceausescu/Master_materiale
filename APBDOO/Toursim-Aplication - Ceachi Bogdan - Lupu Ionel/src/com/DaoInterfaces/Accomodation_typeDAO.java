package com.DaoInterfaces;

import java.util.List;

import com.Tables.Accomodation_type;

public interface Accomodation_typeDAO {
	   public List<Accomodation_type> getAllAccomodation_types();
	   public Accomodation_type getAccomodation_type(int id);
	   public void updateAccomodation_type(Accomodation_type accomodation_type);
	   public void deleteAccomodation_type(Accomodation_type accomodation_type);
}
