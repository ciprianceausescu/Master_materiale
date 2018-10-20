package com.DaoInterfaces;

import java.util.List;

import com.Tables.Country;
import com.Tables.County;
import com.Tables.Region;

public interface CountyDAO {
	   public List<County> getAllCounties();
	   public County getCounty(int id);
	   
	   
	   public Region getRegion(int region_id);
	   
	   // Update delete
	   public void updateCounty(County county);
	   public void deleteCounty(County county);

}
