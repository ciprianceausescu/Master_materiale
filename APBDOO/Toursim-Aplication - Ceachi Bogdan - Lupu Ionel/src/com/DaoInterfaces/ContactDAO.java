package com.DaoInterfaces;

import java.util.List;

import com.Tables.Accomodation;
import com.Tables.Contact;

public interface ContactDAO {
	  public List<Contact> getAllContacts();
	   public Contact getContact(int id);
	   public void updateContact(Contact contact);
	   public void deleteContact(Contact contact);
}
