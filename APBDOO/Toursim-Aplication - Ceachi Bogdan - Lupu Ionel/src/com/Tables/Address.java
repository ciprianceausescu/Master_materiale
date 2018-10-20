package com.Tables;

import com.DAO.Implementations.Factory;

public class Address {
	

	private int id;
	private String name;
	private String postal_code;
	private double latitude;
	private double longitude;
	private int city_id;
	
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
	public String getPostal_code() {
		return postal_code;
	}
	public void setPostal_code(String postal_code) {
		this.postal_code = postal_code;
	}
	public double getLatitude() {
		return latitude;
	}
	public void setLatitude(double latitude) {
		this.latitude = latitude;
	}
	public double getLongitude() {
		return longitude;
	}
	public void setLongitude(double longitude) {
		this.longitude = longitude;
	}
	public int getCity_id() {
		return city_id;
	}
	public void setCity_id(int city_id) {
		this.city_id = city_id;
	}
	
	public City getCity() {
		return Factory.getCityImpl().getCity(this.getCity_id());
	}

	@Override
	public String toString() {
		return "Address [id=" + id + ", name=" + name + ", postal_code=" + postal_code + ", latitude=" + latitude
				+ ", longitude=" + longitude + ", city_id=" + city_id + "]";
	}

}
