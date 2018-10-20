package com.DaoInterfaces;

import java.util.List;

import com.Tables.City;
import com.Tables.Country;
import com.Tables.County;

public interface CityDAO {
	   public List<City> getAllCities();
	   public City getCity(int id);
	   
	   public County getCounty(int county_id);
	   
	   
	   
	   public void updateCity(City city);
	   public void deleteCity(City city);

}
