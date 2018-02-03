package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.JocDAO;
import com.Tables.Angajat;
import com.Tables.Joc;
import com.dbConnection.MySQLConnection;

public class JocImpl implements JocDAO {
	public List<Joc> getAllGames() {
		String sql = "Select * from joc order by joc_id asc";
		List<Joc> gameList = new ArrayList<Joc>();

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			ResultSet rs = pstm.executeQuery();

			while (rs.next()) {
				Joc joc = new Joc();

				// get the parameters selected in the table
				int joc_id = rs.getInt("joc_id");
				String nume = rs.getString("nume");
				String imagine = rs.getString("imagine");
				String descriere = rs.getString("descriere");
				long an_publicare = rs.getLong("an_publicare");
				long joc_categorie_id = rs.getLong("joc_categorie_id");
				long joc_producator_id = rs.getLong("joc_producator_id");
				long joc_tip_id = rs.getLong("joc_tip_id");
				long joc_numar_jucatori_id = rs
						.getLong("joc_numar_jucatori_id");

				// add to list
				joc.setJoc_id(joc_id);
				joc.setNume(nume);
				joc.setImagine(imagine);
				joc.setDescriere(descriere);
				joc.setAn_publicare(an_publicare);
				joc.setJoc_categorie_id(joc_categorie_id);
				joc.setJoc_producator_id(joc_producator_id);
				joc.setJoc_tip_id(joc_tip_id);
				joc.setJoc_numar_jucatori_id(joc_numar_jucatori_id);
				// add to list
				gameList.add(joc);
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		// ! remember to return the new List
		return gameList;
	}

	public Joc getGame(int id) {
		String sql = "Select * from joc where joc_id=?";

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
			ResultSet rs = pstm.executeQuery();

			if (rs.next()) {
				Joc joc = new Joc();

				// get the parameters selected in the table
				int joc_id = rs.getInt("joc_id");
				String nume = rs.getString("nume");
				String imagine = rs.getString("imagine");
				String descriere = rs.getString("descriere");
				long an_publicare = rs.getLong("an_publicare");
				long joc_categorie_id = rs.getLong("joc_categorie_id");
				long joc_producator_id = rs.getLong("joc_producator_id");
				long joc_tip_id = rs.getLong("joc_tip_id");
				long joc_numar_jucatori_id = rs
						.getLong("joc_numar_jucatori_id");

				// add to list
				joc.setJoc_id(joc_id);
				joc.setNume(nume);
				joc.setImagine(imagine);
				joc.setDescriere(descriere);
				joc.setAn_publicare(an_publicare);
				joc.setJoc_categorie_id(joc_categorie_id);
				joc.setJoc_producator_id(joc_producator_id);
				joc.setJoc_tip_id(joc_tip_id);
				joc.setJoc_numar_jucatori_id(joc_numar_jucatori_id);

				return joc;
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		// ! remember to return the new List
		return null;
	}

	public void updateGame(Joc joc) {
		String sql = "Update joc set nume=?, descriere=?, joc_categorie_id=?, "
				+ "joc_producator_id=?, joc_tip_id=?, joc_numar_jucatori_id=? where joc_id=? ";

		try {
			Connection conn = MySQLConnection.startConnection();
			PreparedStatement pstm = conn.prepareStatement(sql);
			System.out.println("Connected.");
			// implement ? in query
			pstm.setString(1, joc.getNume());
			pstm.setString(2, joc.getDescriere());
			pstm.setLong(3, joc.getJoc_categorie_id());
			pstm.setLong(4, joc.getJoc_producator_id());
			pstm.setLong(5, joc.getJoc_tip_id());
			pstm.setLong(6, joc.getJoc_numar_jucatori_id());
			pstm.setInt(7, joc.getJoc_id());

			// execute
			pstm.executeUpdate();
			pstm.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
	}
	public List<Joc> searchGame(String nume){
		String sql = "Select * from joc where upper(nume) like upper(?) order by joc_id asc";
		List<Joc> gameList = new ArrayList<Joc>();

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setString(1, nume);
			ResultSet rs = pstm.executeQuery();

			while (rs.next()) {
				Joc joc = new Joc();

				// get the parameters selected in the table
				int joc_id = rs.getInt("joc_id");
				String numeJoc = rs.getString("nume"); 
				String imagine = rs.getString("imagine");
				String descriere = rs.getString("descriere");
				long an_publicare = rs.getLong("an_publicare");
				long joc_categorie_id = rs.getLong("joc_categorie_id");
				long joc_producator_id = rs.getLong("joc_producator_id");
				long joc_tip_id = rs.getLong("joc_tip_id");
				long joc_numar_jucatori_id = rs
						.getLong("joc_numar_jucatori_id");

				// add to list
				joc.setJoc_id(joc_id);
				joc.setNume(numeJoc);
				joc.setImagine(imagine);
				joc.setDescriere(descriere);
				joc.setAn_publicare(an_publicare);
				joc.setJoc_categorie_id(joc_categorie_id);
				joc.setJoc_producator_id(joc_producator_id);
				joc.setJoc_tip_id(joc_tip_id);
				joc.setJoc_numar_jucatori_id(joc_numar_jucatori_id);
				// add to list
				gameList.add(joc);
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		// ! remember to return the new List
		return gameList;
	}
	public void deleteGame(int id){
		String sql = "Delete from joc where joc_id = ?";

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
			ResultSet rs = pstm.executeQuery();

			pstm.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
	}
	public void addGame(Joc joc) {
		String sql = "insert into joc(joc_id, nume, imagine, descriere, an_publicare, joc_categorie_id, joc_producator_id, joc_tip_id,"
				+ "joc_numar_jucatori_id) values (?,?,?,?,?,?,?,?,?)";
		String sqlIdentifier = "select s_joc_id.NEXTVAL from dual";
		int joc_id = 0;
		
		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pst = conn.prepareStatement(sqlIdentifier);
			ResultSet rs = pst.executeQuery();
			if(rs.next())
				joc_id = rs.getInt(1);
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, joc_id);
			pstm.setString(2, joc.getNume());
			if(joc.getImagine()=="")
				pstm.setString(3,"http://www.underconsideration.com/wordit/wordit_archives/0401_empty_Darrel_Austin.jpg");
			else
				pstm.setString(3, joc.getImagine());
			pstm.setString(4, joc.getDescriere());
			pstm.setLong(5, joc.getAn_publicare());
			pstm.setLong(6, joc.getJoc_categorie_id());
			pstm.setLong(7, joc.getJoc_producator_id());
			pstm.setLong(8, joc.getJoc_tip_id());
			pstm.setLong(9, joc.getJoc_numar_jucatori_id());
			pstm.executeQuery();
			rs.close();
		}
		catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
	}
}
