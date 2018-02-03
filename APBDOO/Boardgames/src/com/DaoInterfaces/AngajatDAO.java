package com.DaoInterfaces;

import java.util.List;

import com.Tables.Angajat;
public interface AngajatDAO {
	   public List<Angajat> getAllEmployees();
	   public Angajat getEmployee(int id);
	   public void updateEmployee(Angajat employee);
	   public void deleteEmployee(int id);
	   public List<Angajat> searchEmployee(String nume);
	   public void addEmployee(Angajat employee);
}
