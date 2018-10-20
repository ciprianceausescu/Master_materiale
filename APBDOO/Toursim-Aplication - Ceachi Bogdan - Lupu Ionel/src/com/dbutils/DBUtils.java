package com.dbutils;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import com.Tables.*;


public class DBUtils {
	public static boolean checkEmployee(Connection conn, String userName, String password) throws SQLException {
		 
        String sql = "Select * from employee where first_name = ? and password = ?";
 
        PreparedStatement pstm = conn.prepareStatement(sql);
        pstm.setString(1, userName);
        pstm.setString(2, password);
 
        ResultSet rs = pstm.executeQuery();
 
        if (rs.next()) {
            return true;
        }
        return false;
    }
	
	public static List<Country> findCountry(Connection conn, String name) throws SQLException {
		String sql = "Select id, name from country where name = ?";
		 
        PreparedStatement pstm = conn.prepareStatement(sql);
        ResultSet rs = pstm.executeQuery();
        
        List<Country> countryList = new ArrayList<Country>();
        
        if(rs.next()) {
        	Country country = new Country();
        	int countryID = rs.getInt("id");
        	String countryName = rs.getString("name");
        	country.setId(countryID);
        	country.setName(countryName);
        	countryList.add(country);
        }
		return countryList;
	}
	public static List<Country> getAllCountries(Connection conn) throws SQLException {
		String sql = "Select * from country";
		 
        PreparedStatement pstm = conn.prepareStatement(sql);
        ResultSet rs = pstm.executeQuery();
        
        List<Country> countryList = new ArrayList<Country>();
        
        while(rs.next()) {
        	Country country = new Country();
        	int countryID = rs.getInt("id");
        	String countryName = rs.getString("name");
        	country.setId(countryID);
        	country.setName(countryName);
        	countryList.add(country);
        }
		return countryList;
	}
	
/*
	public static List<Accomodation> getAllAccomodations(Connection conn) throws SQLException {
		String sql = "Select *  from accomodation ";
		
		 PreparedStatement pstm = conn.prepareStatement(sql);
	     ResultSet rs = pstm.executeQuery();
		
		List<Accomodation> list = new ArrayList<Accomodation>();
		
		while (rs.next()) {
			 int id = rs.getInt("id");
			 String name = rs.getString("name");
			 String stars = rs.getString("stars");
			 String description_full = rs.getString("description_full");
			 String description_short = rs.getString("description_short");
			 int adress_id = rs.getInt("adress_id");
			 int contact_id = rs.getInt("contact_id");
			 int accomodation_type_id = rs.getInt("accomodation_type_id");
            Accomodation accomodation = new Accomodation();
            accomodation.setId(id);
            accomodation.setName(name);
            accomodation.setStars(stars);
            accomodation.setDescription_full(description_full);
            accomodation.setDescription_short(description_short);
            accomodation.setAdress_id(adress_id);
            accomodation.setContact_id(contact_id);
            accomodation.setAccomodation_type_id(accomodation_type_id);        
            list.add(accomodation);
        }
        return list;
	}
	*/
}
