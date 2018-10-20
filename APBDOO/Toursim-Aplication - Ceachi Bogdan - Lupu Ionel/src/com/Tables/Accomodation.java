package com.Tables;

import java.util.List;

import com.DAO.Implementations.Factory;


public class Accomodation {
	private int id;
	private String name;
	private int stars;
	private String description_full;
	private String description_short;
	//private List<Address> address;
	//private List<Contact> contact;
	private int address_id;
	private int contact_id;
	private int accomodation_type_id;
	
	
	public int getAddress_id() {
		return address_id;
	}
	public void setAddress_id(int address_id) {
		this.address_id = address_id;
	}
	public int getContact_id() {
		return contact_id;
	}
	public void setContact_id(int contact_id) {
		this.contact_id = contact_id;
	}
	public int getAccomodation_type_id() {
		return accomodation_type_id;
	}
	public void setAccomodation_type_id(int accomodation_type_id) {
		this.accomodation_type_id = accomodation_type_id;
	}
	public int getId() {
		return id;
	}
	public void setId(int id) {
		this.id = id;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public int getStars() {
		return stars;
	}
	public void setStars(int stars) {
		this.stars = stars;
	}
	public String getDescription_full() {
		return description_full;
	}
	public void setDescription_full(String description_full) {
		this.description_full = description_full;
	}
	public String getDescription_short() {
		return description_short;
	}
	public void setDescription_short(String description_short) {
		this.description_short = description_short;
	}
	
	@Override
	public String toString() {
		return "Accomodation [id=" + id + ", name=" + name + ", stars=" + stars + ", description_full="
				+ description_full + ", description_short=" + description_short + ", adress_id=" + address_id
				+ ", contact_id=" + contact_id + ", accomodation_type_id=" + accomodation_type_id+"]";
	}
	

	public Address getAddress() {
		return Factory.getAddressImpl().getAddress(this.getAddress_id());
	}
	
	public Contact getContact() {
		return Factory.getContactImpl().getContact(this.getContact_id());
	}
	
	public Accomodation_type getType() {
		return Factory.getAccomodation_typeImpl().getAccomodation_type(this.getAccomodation_type_id());
	}
	
	
	
}
