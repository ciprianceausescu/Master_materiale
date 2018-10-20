package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;
import com.Tables.*;
import com.Tables.Address;
import com.dbConnection.MySQLConnection;
import com.DaoInterfaces.AccomodationDAO;
import com.DaoInterfaces.DepartmentDAO;


public class DepartmentImpl implements DepartmentDAO {

	@Override
	public List<Department> getAllDepartments() {
		String sql = "Select * from department";
		List<Department> departmentList = new ArrayList<Department>();
				
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
	        ResultSet rs = pstm.executeQuery();
	        
	        while(rs.next()) {
	        	Department department = new Department();
	        	
	        	// get the parameters selected in the table
	        	int departmentID = rs.getInt("id");
	        	String departmentName = rs.getString("name");
	        	int address_id = rs.getInt("address_id");
	        	
	        	// add to list
	        	department.setId(departmentID);
	        	department.setName(departmentName);
	        	department.setAddress_id(address_id);
	        	
	        	// add to list
	        	departmentList.add(department);
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		// ! remember to return the new List
		return departmentList;
	}

	@Override
	public Department getDepartment(int id) {
		String sql = "Select * from department where id=?";
		
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
	        ResultSet rs = pstm.executeQuery();
	        
	        if(rs.next()) {
	        	Department department = new Department();
	        	
	        	// get the parameters selected in the table
	        	int departmentID = rs.getInt("id");
	        	String departmentName = rs.getString("name");
	        	int address_id = rs.getInt("address_id");
	        	
	        	// add to list
	        	department.setId(departmentID);
	        	department.setName(departmentName);
	        	department.setAddress_id(address_id);
	        	
	        	// return
	        	return department;
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public void updateDepartment(Department department) {
		String sql = "Update department set name =?,address_id=? where id=? ";
		 
		try {
		Connection conn = MySQLConnection.startConnection();
        PreparedStatement pstm = conn.prepareStatement(sql);
 
        // implement ? in query
        pstm.setString(1, department.getName());
        pstm.setInt(2, department.getAddress_id());
        pstm.setInt(3, department.getId());
        
          
        //execute
        pstm.executeUpdate();
        pstm.close();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public void deleteDepartment(Department department) {
		String sql = "Delete From Department where id= ?";
		 
		try {
			Connection conn = MySQLConnection.startConnection();
	        PreparedStatement pstm = conn.prepareStatement(sql);
	        
	        pstm.setInt(1, department.getId());
	        pstm.executeUpdate();
		}catch(Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public Address getAddress(int address_id) {
		Address address = Factory.getAddressImpl().getAddress(address_id);
		return address;
	}

	

}
