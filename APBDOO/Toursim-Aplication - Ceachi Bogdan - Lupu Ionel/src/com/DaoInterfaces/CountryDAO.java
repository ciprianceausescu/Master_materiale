package com.DaoInterfaces;

import java.sql.Connection;
import java.util.List;

import com.Tables.Country;
import com.Tables.Region;

public interface CountryDAO {
	   public List<Country> getAllCountries();
	   public Country getCountry(int id);
	   public Country getCountry(String name); // Consider ca este unic?
	   
	   public List<Region> getAllCountryRegions(Country country);
	   
	   public void updateCountry(Country country);
	   public void deleteCountry(Country country);
	   
	   
	   
}
