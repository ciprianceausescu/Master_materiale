package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.CountryDAO;
import com.Tables.Country;
import com.Tables.Region;
import com.dbConnection.MySQLConnection;

public class CountryImpl implements CountryDAO {
	
	@Override
	public List<Country> getAllCountries() {
		String sql = "Select * from country";
		List<Country> countryList = new ArrayList<Country>();
				
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
	        ResultSet rs = pstm.executeQuery();
	        
	        while(rs.next()) {
	        	Country country = new Country();
	        	int countryID = rs.getInt("id");
	        	String countryName = rs.getString("name");
	        	country.setId(countryID);
	        	country.setName(countryName);
	        	countryList.add(country);
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return countryList;
	}

	@Override
	public Country getCountry(int id) {
		String sql = "Select * from country where id=?";
		
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
	        ResultSet rs = pstm.executeQuery();
	        
	        if(rs.next()) {
	        	Country country = new Country();
	        	int countryID = rs.getInt("id");
	        	String countryName = rs.getString("name");
	        	country.setId(countryID);
	        	country.setName(countryName);
	        	return country;
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	@Override
	public Country getCountry(String name) {
		String sql = "Select * from country where name=?";
				
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setString(1, name);
	        ResultSet rs = pstm.executeQuery();
	        
	        if(rs.next()) {
	        	Country country = new Country();
	        	int countryID = rs.getInt("id");
	        	String countryName = rs.getString("name");
	        	country.setId(countryID);
	        	country.setName(countryName);
	        	return country;
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public void updateCountry(Country country) {
		String sql = "Update country set name =? where id=? ";
		 
		try {
		Connection conn = MySQLConnection.startConnection();
        PreparedStatement pstm = conn.prepareStatement(sql);
 
        pstm.setString(1, country.getName());
        pstm.setInt(2, country.getId());
        pstm.executeUpdate();
        
        pstm.close();
		}catch(Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void deleteCountry(Country country) {
		String sql = "Delete From Country where id= ?";
		 
		try {
			Connection conn = MySQLConnection.startConnection();
	        PreparedStatement pstm = conn.prepareStatement(sql);
	        
	        pstm.setInt(1, country.getId());
	        pstm.executeUpdate();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public List<Region> getAllCountryRegions(Country country) {
		String sql = "select r.id, r.name, r.country_id from "
				+ "region r left join country c on c.id = r.country_id where c.name = ?";
		List<Region> regionList = new ArrayList<Region>();
		
		
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setString(1, country.getName());
			
	        ResultSet rs = pstm.executeQuery();
	        
	        while(rs.next()) {
	        	Region region = new Region();
	        	int regionID = rs.getInt("id");
	        	String regionName = rs.getString("name");
	        	int regionCountry_id = rs.getInt("country_id");
	        	region.setId(regionID);
	        	region.setName(regionName);
	        	region.setCountry_id(regionCountry_id);
	        	regionList.add(region);
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return regionList;
	}
	
	
	

}
