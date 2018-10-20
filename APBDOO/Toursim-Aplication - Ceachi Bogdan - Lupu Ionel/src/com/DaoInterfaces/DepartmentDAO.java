package com.DaoInterfaces;

import java.util.List;

import com.Tables.Country;
import com.Tables.Department;
import com.Tables.Address;

public interface DepartmentDAO {
	   public List<Department> getAllDepartments();
	   public Department getDepartment(int id);
	   
	   public Address getAddress(int address_id);
	   
	   
	   
	   public void updateDepartment(Department department);
	   public void deleteDepartment(Department department);

}
