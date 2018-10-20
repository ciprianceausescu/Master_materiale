package com.DAO.Implementations;

import java.sql.Connection;
import com.Tables.*;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.EmployeeDAO;
import com.dbConnection.MySQLConnection;

public class EmployeeImpl implements EmployeeDAO {
		
	@Override
	public List<Employee> getAllEmployees() {
		String sql = "Select * from employee";
		List<Employee> employeeList = new ArrayList<Employee>();
				
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
	        ResultSet rs = pstm.executeQuery();
	        
	        while(rs.next()) {
	        	Employee employee = new Employee();
	        	
	        	// get the parameters selected in the table
	        	int employeeID = rs.getInt("id");
	        	String employeeFirst_name = rs.getString("first_name");
	        	String employeeLast_name = rs.getString("last_name");
	        	// !!! nu am luat data nasterii
	        	int employeeContact_id = rs.getInt("contact_id");
	        	int employeeSalary = rs.getInt("salary");
	        	int department_id = rs.getInt("department_id");
	        	int job_id = rs.getInt("job_id");
	        	String employeePassword = rs.getString("password");
	        	
	        	// add to list
	        	employee.setId(employeeID);
	        	employee.setFirstName(employeeFirst_name);
	        	employee.setLastName(employeeLast_name);
	        	employee.setContact_id(employeeContact_id);
	        	employee.setSalary(employeeSalary);
	        	employee.setDepartment_id(department_id);
	        	employee.setJob_id(job_id);
	        	employee.setPassword(employeePassword);
	        	
	        	// add to list
	        	employeeList.add(employee);
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		// ! remember to return the new List
		return employeeList;
	}

	@Override
	public Employee getEmployee(int id) {
		String sql = "Select * from employee where id=?";
		
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
	        ResultSet rs = pstm.executeQuery();
	        
	        if(rs.next()) {
	        	Employee employee = new Employee();
	        	
	        	// get the parameters selected in the table
	        	int employeeID = rs.getInt("id");
	        	String employeeFirst_name = rs.getString("first_name");
	        	String employeeLast_name = rs.getString("last_name");
	        	// !!! nu am luat data nasterii
	        	int employeeContact_id = rs.getInt("contact_id");
	        	int employeeSalary = rs.getInt("salary");
	        	int department_id = rs.getInt("department_id");
	        	int job_id = rs.getInt("job_id");
	        	String employeePassword = rs.getString("password");
	        	
	        	// add to list
	        	employee.setId(employeeID);
	        	employee.setFirstName(employeeFirst_name);
	        	employee.setLastName(employeeLast_name);
	        	employee.setContact_id(employeeContact_id);
	        	employee.setSalary(employeeSalary);
	        	employee.setDepartment_id(department_id);
	        	employee.setJob_id(job_id);
	        	employee.setPassword(employeePassword);
	        	
	        	// return
	        	return employee;
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public void updateEmployee(Employee employee) {
		String sql = "Update employee set first_name=?, "
				+ "last_name=?, contact_id=?, salary=?, "
				+ "department_id=?, job_id=?, "
				+ "password=? where id=? ";
		 
		try {
		Connection conn = MySQLConnection.startConnection();
        PreparedStatement pstm = conn.prepareStatement(sql);
 
        // implement ? in query
        pstm.setString(1, employee.getFirstName());
        pstm.setString(2, employee.getLastName());
        pstm.setInt(3, employee.getContact_id());
        pstm.setFloat(4, employee.getSalary());
        pstm.setInt(5, employee.getDepartment_id());
        pstm.setInt(6, employee.getJob_id());
        pstm.setString(7, employee.getPassword());
        pstm.setInt(8, employee.getId());
        
        
          
        //execute
        pstm.executeUpdate();
        pstm.close();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public void deleteEmployee(Employee employee) {
		String sql = "Delete From Employee where id= ?";
		 
		try {
			Connection conn = MySQLConnection.startConnection();
	        PreparedStatement pstm = conn.prepareStatement(sql);
	        
	        pstm.setInt(1, employee.getId());
	        pstm.executeUpdate();
		}catch(Exception e) {
			e.printStackTrace();
		}

	}

	@Override
	public boolean checkEmployee(String userName, String password) {
		String sql = "Select * from employee where first_name = ? and password = ?";
		try {		
			Connection conn = MySQLConnection.startConnection();
			PreparedStatement pstm = conn.prepareStatement(sql);
	        pstm.setString(1, userName);
	        pstm.setString(2, password);
	        ResultSet rs = pstm.executeQuery();
	        
	        if (rs.next()) {
	        	pstm.close();
	            return true;
	        }
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return false;
	}

	@Override
	public Department getDepartment(int department_id) {
		Department department = Factory.getDepartmentImpl().getDepartment(department_id);
		return department;
	}

}
