package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;
import com.Tables.*;
import com.dbConnection.MySQLConnection;
import com.DaoInterfaces.AddressDAO;

public class AddressImpl implements AddressDAO {
	
	
	@Override
	public List<Address> getAllAddress() {
		String sql = "Select * from address";
		List<Address> addressList = new ArrayList<Address>();
				
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
	        ResultSet rs = pstm.executeQuery();
	        
	        while(rs.next()) {
	        	Address address = new Address();
	        	
	        	// get the parameters selected in the table
	        	int addressID = rs.getInt("id");
	        	String addressName = rs.getString("name");
	        	String addressPostalCode = rs.getString("postal_code");
	        	double addressLatitude = rs.getDouble("latitude");
	        	double addressLongitude = rs.getDouble("longitude");
	        	int city_id = rs.getInt("city_id");       	
	        	
	        	// add to list
	        	address.setId(addressID);
	        	address.setName(addressName);
	        	address.setPostal_code(addressPostalCode);
	        	address.setLatitude(addressLatitude);
	        	address.setLongitude(addressLongitude);
	        	address.setCity_id(city_id);
	        	
	        	// add to list
	        	addressList.add(address);
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		// ! remember to return the new List
		return addressList;
	}

	@Override
	public Address getAddress(int id) {
		String sql = "Select * from address where id=?";
		
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
	        ResultSet rs = pstm.executeQuery();
	        
	        if(rs.next()) {
	        	Address address = new Address();
	        	
	        	// get the parameters selected in the table
	        	int addressID = rs.getInt("id");
	        	String addressName = rs.getString("name");
	        	String addressPostalCode = rs.getString("postal_code");
	        	double addressLatitude = rs.getDouble("latitude");
	        	double addressLongitude = rs.getDouble("longitude");
	        	int city_id = rs.getInt("city_id");       	
	        	
	        	// add to list
	        	address.setId(addressID);
	        	address.setName(addressName);
	        	address.setPostal_code(addressPostalCode);
	        	address.setLatitude(addressLatitude);
	        	address.setLongitude(addressLongitude);
	        	address.setCity_id(city_id);
	        	
	        	// return
	        	return address;
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public void updateAddress(Address address) {
		String sql = "Update address set name =?, postal_code=?, latitude=?, longitude=?, city_id=? where id=? ";
		 
		try {
		Connection conn = MySQLConnection.startConnection();
        PreparedStatement pstm = conn.prepareStatement(sql);
 
        // implement ? in query
        pstm.setString(1,address.getName());
        pstm.setString(2, address.getPostal_code());
        pstm.setDouble(3,  address.getLatitude());
        pstm.setDouble(4, address.getLongitude());
        pstm.setInt(5, address.getCity_id());
        pstm.setInt(6, address.getId());
        
          
        //execute
        pstm.executeUpdate();
        pstm.close();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public void deleteAddress(Address address) {
		String sql = "Delete From Address where id= ?";
		 
		try {
			Connection conn = MySQLConnection.startConnection();
	        PreparedStatement pstm = conn.prepareStatement(sql);
	        
	        pstm.setInt(1, address.getId());
	        pstm.executeUpdate();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public City getCity(int city_id) {
		City city = Factory.getCityImpl().getCity(city_id);
		return city;
	}

}
