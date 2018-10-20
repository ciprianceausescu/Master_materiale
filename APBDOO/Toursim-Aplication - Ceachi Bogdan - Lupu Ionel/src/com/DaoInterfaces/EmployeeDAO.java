package com.DaoInterfaces;

import java.util.List;

import com.Tables.Country;
import com.Tables.Department;
import com.Tables.Employee;

public interface EmployeeDAO {
	   public List<Employee> getAllEmployees();
	   public Employee getEmployee(int id);
	   
	   
	   public Department getDepartment(int department_id);
	   
	   
	   public void updateEmployee(Employee employee);
	   public void deleteEmployee(Employee employee);
	   
	   
	   public boolean checkEmployee(String userName, String password);

}
