package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.Tables.*;
import com.dbConnection.MySQLConnection;
import com.DaoInterfaces.CityDAO;

public class CityImpl implements CityDAO {

	@Override
	public List<City> getAllCities() {
		String sql = "Select * from city";
		List<City> cityList = new ArrayList<City>();
				
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
	        ResultSet rs = pstm.executeQuery();
	        
	        while(rs.next()) {
	        	City city = new City();
	        	
	        	// get the parameters selected in the table
	        	int cityID = rs.getInt("id");
	        	String cityName = rs.getString("name");
	        	String cityISC3 = rs.getString("ISC3");// daca apare vreo eroare, poate ISC3 e cu litere mici
	        	double cityLatitude = rs.getDouble("latitude");
	        	double cityLongitude = rs.getDouble("longitude");
	        	int county_id = rs.getInt("county_id");
	        	
	        	// add to list
	        	city.setId(cityID);
	        	city.setName(cityName);
	        	city.setISC3(cityISC3);
	        	city.setLatitude(cityLatitude);
	        	city.setLongitude(cityLongitude);
	        	city.setCounty_id(county_id);
	        	
	        	// add to list
	        	cityList.add(city);
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		// ! remember to return the new List
		return cityList;
	}

	@Override
	public City getCity(int id) {
		String sql = "Select * from city where id=?";
		
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
	        ResultSet rs = pstm.executeQuery();
	        
	        if(rs.next()) {
	        	City city = new City();
	        	
	        	// get the parameters selected in the table
	        	int cityID = rs.getInt("id");
	        	String cityName = rs.getString("name");
	        	String cityISC3 = rs.getString("ISO3");// daca apare vreo eroare, poate ISC3 e cu litere mici
	        	double cityLatitude = rs.getDouble("latitude");
	        	double cityLongitude = rs.getDouble("longitude");
	        	int county_id = rs.getInt("county_id");
	        	
	        	// add to list
	        	city.setId(cityID);
	        	city.setName(cityName);
	        	city.setISC3(cityISC3);
	        	city.setLatitude(cityLatitude);
	        	city.setLongitude(cityLongitude);
	        	city.setCounty_id(county_id);
	        	
	        	// return
	        	return city;
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public void updateCity(City city) {
		String sql = "Update city set name =?, ISC3=?, latitude=?, longitude=?, county_id=? where id=? ";
		 
		try {
		Connection conn = MySQLConnection.startConnection();
        PreparedStatement pstm = conn.prepareStatement(sql);
 
        // implement ? in query
        pstm.setString(1, city.getName());
        pstm.setString(2, city.getISC3());
        pstm.setDouble(3, city.getLatitude());
        pstm.setDouble(4, city.getLongitude());
        pstm.setInt(5, city.getCounty_id());
        pstm.setInt(6, city.getId());
          
        //execute
        pstm.executeUpdate();
        pstm.close();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public void deleteCity(City city) {
		String sql = "Delete From City where id= ?";
		 
		try {
			Connection conn = MySQLConnection.startConnection();
	        PreparedStatement pstm = conn.prepareStatement(sql);
	        
	        pstm.setInt(1, city.getId());
	        pstm.executeUpdate();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public County getCounty(int county_id) {
		County county = Factory.getCountyImpl().getCounty(county_id);
		return county;
	}

}
