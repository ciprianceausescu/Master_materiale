package com.DAO.Implementations;

import java.sql.Connection;

import com.Tables.*;

import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.AngajatDAO;
import com.dbConnection.MySQLConnection;

public class AngajatImpl implements AngajatDAO {

	@Override
	public List<Angajat> getAllEmployees() {
		String sql = "Select * from angajat order by angajat_id asc";
		List<Angajat> employeeList = new ArrayList<Angajat>();

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			ResultSet rs = pstm.executeQuery();

			while (rs.next()) {
				Angajat angajat = new Angajat();

				// get the parameters selected in the table
				int angajat_id = rs.getInt("angajat_id");
				String nume = rs.getString("nume");
				long cnp = rs.getLong("cnp");
				int adresa = rs.getInt("adresa");
				long telefon = rs.getLong("telefon");
				int magazin_departament_id = rs
						.getInt("magazin_departament_id");
				int angajat_contract_id = rs.getInt("angajat_contract_id");
				int angajat_pozitie_id = rs.getInt("angajat_pozitie_id");
				// add to list
				angajat.setAngajat_id(angajat_id);
				angajat.setNume(nume);
				angajat.setCNP(cnp);
				angajat.setAdresa(adresa);
				angajat.setTelefon(telefon);
				angajat.setMagazin_departament_id(magazin_departament_id);
				angajat.setAngajat_contract_id(angajat_contract_id);
				angajat.setAngajat_pozitie_id(angajat_pozitie_id);
				// add to list
				employeeList.add(angajat);
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		// ! remember to return the new List
		return employeeList;
	}

	@Override
	public Angajat getEmployee(int id) {
		String sql = "Select * from angajat where angajat_id=?";

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
			ResultSet rs = pstm.executeQuery();

			if (rs.next()) {
				Angajat angajat = new Angajat();

				// get the parameters selected in the table
				int angajat_id = rs.getInt("angajat_id");
				String nume = rs.getString("nume");
				long cnp = rs.getLong("cnp");
				int adresa = rs.getInt("adresa");
				long telefon = rs.getLong("telefon");
				int magazin_departament_id = rs
						.getInt("magazin_departament_id");
				int angajat_contract_id = rs.getInt("angajat_contract_id");
				int angajat_pozitie_id = rs.getInt("angajat_pozitie_id");

				// add to list
				angajat.setAngajat_id(angajat_id);
				angajat.setNume(nume);
				angajat.setCNP(cnp);
				angajat.setAdresa(adresa);
				angajat.setTelefon(telefon);
				angajat.setMagazin_departament_id(magazin_departament_id);
				angajat.setAngajat_contract_id(angajat_contract_id);
				angajat.setAngajat_pozitie_id(angajat_pozitie_id);

				// return
				return angajat;
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		return null;
	}

	/*
	 * @Override public Department getDepartment(int department_id) { // TODO
	 * Auto-generated method stub return null; }
	 */

	@Override
	public void updateEmployee(Angajat employee) {
		String sql = "Update angajat set nume=?, telefon=? where angajat_id =? ";

		try {
			Connection conn = MySQLConnection.startConnection();
			PreparedStatement pstm = conn.prepareStatement(sql);

			// implement ? in query
			pstm.setString(1, employee.getNume());
			pstm.setLong(2, employee.getTelefon());
			pstm.setInt(3, employee.getAngajat_id());

			// execute
			pstm.executeUpdate();
			pstm.close();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void deleteEmployee(int id) {
		String sql = "Delete From angajat where angajat_id = ?";
		try {
			Connection conn = MySQLConnection.startConnection();
			PreparedStatement pstm = conn.prepareStatement(sql);

			pstm.setInt(1, id);
			pstm.executeUpdate();
			pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	public List<Angajat> searchEmployee(String nume){
		String sql = "Select * from angajat where upper(nume) like upper(?) order by angajat_id asc";
		List<Angajat> employeeList = new ArrayList<Angajat>();

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setString(1, nume);
			ResultSet rs = pstm.executeQuery();

			while (rs.next()) {
				Angajat angajat = new Angajat();

				// get the parameters selected in the table
				int angajat_id = rs.getInt("angajat_id");
				String numeAngajat = rs.getString("nume");
				long cnp = rs.getLong("cnp");
				int adresa = rs.getInt("adresa");
				long telefon = rs.getLong("telefon");
				int magazin_departament_id = rs.getInt("magazin_departament_id");
				int angajat_contract_id = rs.getInt("angajat_contract_id");
				int angajat_pozitie_id = rs.getInt("angajat_pozitie_id");

				// add to list
				angajat.setAngajat_id(angajat_id);
				angajat.setNume(numeAngajat);
				angajat.setCNP(cnp);
				angajat.setAdresa(adresa);
				angajat.setTelefon(telefon);
				angajat.setMagazin_departament_id(magazin_departament_id);
				angajat.setAngajat_contract_id(angajat_contract_id);
				angajat.setAngajat_pozitie_id(angajat_pozitie_id);
				// add to list
				employeeList.add(angajat);
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		// ! remember to return the new List
		return employeeList;
	}
	public void addEmployee(Angajat employee) {
		String sql = "insert into angajat(angajat_id, nume, cnp, adresa, telefon, magazin_departament_id, angajat_contract_id,"
				+ "angajat_pozitie_id) values (?,?,?,?,?,?,?,?)";
		String sqlIdentifier = "select s_angajat_id.NEXTVAL from dual";
		int angajat_id = 0;
		
		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pst = conn.prepareStatement(sqlIdentifier);
			ResultSet rs = pst.executeQuery();
			if(rs.next())
				angajat_id = rs.getInt(1);
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, angajat_id);
			pstm.setString(2, employee.getNume());
			pstm.setLong(3, employee.getCNP());
			pstm.setLong(4, employee.getAdresa());
			pstm.setLong(5, employee.getTelefon());
			pstm.setLong(6, employee.getMagazin_departament_id());
			pstm.setLong(7, employee.getAngajat_contract_id());
			pstm.setLong(8, employee.getAngajat_pozitie_id());
			pstm.executeQuery();
			rs.close();
		}
		catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
	}
}
