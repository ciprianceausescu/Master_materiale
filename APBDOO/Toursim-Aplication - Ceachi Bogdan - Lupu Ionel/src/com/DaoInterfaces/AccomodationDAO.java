package com.DaoInterfaces;

import java.util.List;

import com.Tables.Accomodation;
import com.Tables.Accomodation_type;
import com.Tables.Address;
import com.Tables.Contact;
import com.Tables.Country;

public interface AccomodationDAO {
	   public List<Accomodation> getAllAccomodations();
	   public Accomodation getAccomodation(int id);
	   
	   public Address getAddress(int address_id);
	   public Contact getContact(int contact_id);
	   public Accomodation_type getAccomodation_type(int accomodation_type_id);
	   

	   public Accomodation insertAccomodation(Accomodation accomodation);
	   public void updateAccomodation(Accomodation accomodation);
	   public void deleteAccomodation(Accomodation accomodation);

}
