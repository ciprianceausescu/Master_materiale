package com.Tables;

public class Region {
	public int id;
	public String name;
	public int country_id;
	
	
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
	public int getCountry_id() {
		return country_id;
	}
	public void setCountry_id(int country_id) {
		this.country_id = country_id;
	}
	@Override
	public String toString() {
		return "Region [id=" + id + ", name=" + name + ", country_id=" + country_id + "]";
	}
	
	
	
	

}
