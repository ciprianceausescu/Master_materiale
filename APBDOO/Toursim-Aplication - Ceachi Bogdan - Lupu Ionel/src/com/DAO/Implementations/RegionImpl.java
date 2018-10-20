package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.RegionDAO;
import com.Tables.Country;
import com.Tables.Region;
import com.dbConnection.MySQLConnection;
import com.DAO.Implementations.*;

public class RegionImpl implements RegionDAO {

	@Override
	public List<Region> getAllRegions() {
		String sql = "Select * from region";
		List<Region> regionList = new ArrayList<Region>();
				
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
	        ResultSet rs = pstm.executeQuery();
	        
	        while(rs.next()) {
	        	Region region = new Region();
	        	int regionID = rs.getInt("id");
	        	String regionName = rs.getString("name");
	        	int country_id = rs.getInt("country_id");
	        	
	        	region.setId(regionID);
	        	region.setName(regionName);
	        	region.setCountry_id(country_id);
	        	
	        	regionList.add(region);
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return regionList;
	}

	@Override
	public Region getRegion(int id) {
		String sql = "Select * from region where id=?";
		
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
	        ResultSet rs = pstm.executeQuery();
	        
	        if(rs.next()) {
	        	Region region = new Region();
	        	
	        	int regionID = rs.getInt("id");
	        	String regionName = rs.getString("name");
	        	int country_id = rs.getInt("country_id");
	        	
	        	region.setId(regionID);
	        	region.setName(regionName);
	        	region.setCountry_id(country_id);
	        	return region;
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	@Override
	public Region getRegion(String name) {
		// TODO Auto-generated method stub
		return null;
	}
	
	
	@Override
	public Country getCountry(int country_id) {
		Country country = Factory.getCountryImpl().getCountry(country_id);
		return country;
	}

	@Override
	public void updateRegion(Region region) {
		String sql = "Update region set name =?, country_id=? where id=? ";
		 
		try {
		Connection conn = MySQLConnection.startConnection();
        PreparedStatement pstm = conn.prepareStatement(sql);
 
        pstm.setString(1, region.getName());
        pstm.setInt(2, region.getCountry_id());
        pstm.setInt(3, region.getId());
        
        pstm.executeUpdate();
        pstm.close();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public void deleteRegion(Region region) {
		String sql = "Delete From Region where id= ?";
		 
		try {
			Connection conn = MySQLConnection.startConnection();
	        PreparedStatement pstm = conn.prepareStatement(sql);
	        
	        pstm.setInt(1, region.getId());
	        pstm.executeUpdate();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}	

}
