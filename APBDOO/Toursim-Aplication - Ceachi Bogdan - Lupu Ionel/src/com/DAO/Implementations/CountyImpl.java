package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.CountyDAO;

import com.Tables.*;
import com.dbConnection.MySQLConnection;
public class CountyImpl implements CountyDAO {

	@Override
	public List<County> getAllCounties() {
		String sql = "Select * from county";
		List<County> countyList = new ArrayList<County>();
				
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
	        ResultSet rs = pstm.executeQuery();
	        
	        while(rs.next()) {
	        	County county = new County();
	        	int countyID = rs.getInt("id");
	        	String countyName = rs.getString("name");
	        	int region_id = rs.getInt("region_id");
	        	
	        	county.setId(countyID);
	        	county.setName(countyName);
	        	county.setRegion_id(region_id);
	        	
	        	countyList.add(county);
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return countyList;
	}

	@Override
	public County getCounty(int id) {
		String sql = "Select * from county where id=?";
		
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
	        ResultSet rs = pstm.executeQuery();
	        
	        if(rs.next()) {
	        	County county = new County();
	        	int countyID = rs.getInt("id");
	        	String countyName = rs.getString("name");
	        	int region_id = rs.getInt("region_id");
	        	
	        	county.setId(countyID);
	        	county.setName(countyName);
	        	county.setRegion_id(region_id);
	        	
	        	// return
	        	return county;
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public void updateCounty(County county) {
		String sql = "Update county set name =?, region_id=? where id=? ";
		 
		try {
		Connection conn = MySQLConnection.startConnection();
        PreparedStatement pstm = conn.prepareStatement(sql);
 
        // implement ? in query
        pstm.setString(1, county.getName());
        pstm.setInt(2, county.getRegion_id());
        pstm.setInt(3, county.getId());
        
        //execute
        pstm.executeUpdate();
        pstm.close();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public void deleteCounty(County county) {
		String sql = "Delete From County where id= ?";
		 
		try {
			Connection conn = MySQLConnection.startConnection();
	        PreparedStatement pstm = conn.prepareStatement(sql);
	        
	        pstm.setInt(1, county.getId());
	        pstm.executeUpdate();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public Region getRegion(int region_id) {
		Region region = Factory.getRegionImpl().getRegion(region_id);
		return region;
	}

}
