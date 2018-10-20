package com.DaoInterfaces;

import java.util.List;

import com.Tables.Country;
import com.Tables.Region;

public interface RegionDAO {
	   public List<Region> getAllRegions();
	   
	   // This class getters
	   public Region getRegion(int id);
	   public Region getRegion(String name);
	   
	   //FK getters
	   public Country getCountry(int country_id);
	   
	   
	   //Update, delete
	   public void updateRegion(Region region);
	   public void deleteRegion(Region region);
}
