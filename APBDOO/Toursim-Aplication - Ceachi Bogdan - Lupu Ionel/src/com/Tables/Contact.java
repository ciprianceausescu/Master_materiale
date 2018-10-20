package com.Tables;

public class Contact {
	private int id;
	private String email;
	private String phone;
	private String phone2;
	private String fax;
	private String website;
	private String facebook;
	private String linkedin;
	public int getId() {
		return id;
	}
	public void setId(int id) {
		this.id = id;
	}
	public String getEmail() {
		return email;
	}
	public void setEmail(String email) {
		this.email = email;
	}
	public String getPhone() {
		return phone;
	}
	public void setPhone(String phone) {
		this.phone = phone;
	}
	public String getPhone2() {
		return phone2;
	}
	public void setPhone2(String phone2) {
		this.phone2 = phone2;
	}
	public String getFax() {
		return fax;
	}
	public void setFax(String fax) {
		this.fax = fax;
	}
	public String getWebsite() {
		return website;
	}
	public void setWebsite(String website) {
		this.website = website;
	}
	public String getFacebook() {
		return facebook;
	}
	public void setFacebook(String facebook) {
		this.facebook = facebook;
	}
	public String getLinkedin() {
		return linkedin;
	}
	public void setLinkedin(String linkedin) {
		this.linkedin = linkedin;
	}
	@Override
	public String toString() {
		return "Contact [id=" + id + ", email=" + email + ", phone=" + phone + ", phone2=" + phone2 + ", fax=" + fax
				+ ", website=" + website + ", facebook=" + facebook + ", linkedin=" + linkedin + "]";
	}
	
	
	

}
