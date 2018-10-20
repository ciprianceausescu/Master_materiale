package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;
import com.Tables.*;
import com.dbConnection.MySQLConnection;
import com.DaoInterfaces.Accomodation_typeDAO;

public class Accomodation_typeImpl implements Accomodation_typeDAO {

	@Override
	public List<Accomodation_type> getAllAccomodation_types() {
		String sql = "Select * from accomodation_type";
		List<Accomodation_type> accomodation_typeList = new ArrayList<Accomodation_type>();
				
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
	        ResultSet rs = pstm.executeQuery();
	        
	        while(rs.next()) {
	        	Accomodation_type accomodation_type = new Accomodation_type();
	        	
	        	// get the parameters selected in the table
	        	int accomodation_typeID = rs.getInt("id");
	        	String accomodationName = rs.getString("name");
	        	
	        	// add to list
	        	accomodation_type.setId(accomodation_typeID);
	        	accomodation_type.setName(accomodationName);
	        	
	        	// add to list
	        	accomodation_typeList.add(accomodation_type);
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		// ! remember to return the new List
		return accomodation_typeList;
	}

	@Override
	public Accomodation_type getAccomodation_type(int id) {
		String sql = "Select * from accomodation_type where id=?";
		
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
	        ResultSet rs = pstm.executeQuery();
	        
	        if(rs.next()) {
	        	Accomodation_type accomodation_type = new Accomodation_type();
	        	
	        	// get the parameters selected in the table
	        	int accomodation_typeID = rs.getInt("id");
	        	String accomodationName = rs.getString("name");
	        	
	        	// add to list
	        	accomodation_type.setId(accomodation_typeID);
	        	accomodation_type.setName(accomodationName);
	        	
	        	// return
	        	return accomodation_type;
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public void updateAccomodation_type(Accomodation_type accomodation_type) {
		String sql = "Update accomodation_type set name=? where id=? ";
		 
		try {
		Connection conn = MySQLConnection.startConnection();
        PreparedStatement pstm = conn.prepareStatement(sql);
 
        // implement ? in query
        pstm.setString(1, accomodation_type.getName());
        pstm.setInt(2, accomodation_type.getId());        
          
        //execute
        pstm.executeUpdate();
        pstm.close();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public void deleteAccomodation_type(Accomodation_type accomodation_type) {
		String sql = "Delete From Accomodation_type where id= ?";
		 
		try {
			Connection conn = MySQLConnection.startConnection();
	        PreparedStatement pstm = conn.prepareStatement(sql);
	        
	        pstm.setInt(1, accomodation_type.getId());
	        pstm.executeUpdate();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

}
