package com.Tables;

public class Department {
	private int id;
	private String name;
	private int address_id;
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
	public int getAddress_id() {
		return address_id;
	}
	public void setAddress_id(int address_id) {
		this.address_id = address_id;
	}
	@Override
	public String toString() {
		return "Department [id=" + id + ", name=" + name + ", address_id=" + address_id + "]";
	}
	
	
	

}
