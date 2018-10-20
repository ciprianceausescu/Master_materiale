package com.DAO.Implementations;

import com.Tables.*;

public class Factory {
	public static CountryImpl getCountryImpl() {
		return new CountryImpl();
	}
	
	public static RegionImpl getRegionImpl() {
		return new RegionImpl();
	}
	
	
	public static CountyImpl getCountyImpl() {
		return new CountyImpl();
	}
	
	public static CityImpl getCityImpl() {
		return new CityImpl();
	}
	
	public static AddressImpl getAddressImpl() {
		return new AddressImpl();
	}
	
	public static DepartmentImpl getDepartmentImpl() {
		return new DepartmentImpl();
	}
	
	public static EmployeeImpl getEmployeeImpl() {
		return new EmployeeImpl();
	}
	
	public static AccomodationImpl getAccomodationImpl() {
		return new AccomodationImpl();
	}
	
	public static Accomodation_typeImpl getAccomodation_typeImpl() {
		return new Accomodation_typeImpl();
	}
	
	public static ContactImpl getContactImpl() {
		return new ContactImpl();
	}
	
	
	

}
