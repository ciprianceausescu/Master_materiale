package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.JocCategorieDAO;
import com.Tables.JocCategorie;
import com.Tables.JocProducator;
import com.dbConnection.MySQLConnection;

public class JocCategorieImpl implements JocCategorieDAO {
	public JocCategorie getGameCategory(long id) {
		String sql = "Select * from joc_categorie where joc_categorie_id=?";

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setLong(1, id);
			ResultSet rs = pstm.executeQuery();

			if (rs.next()) {
				JocCategorie jocCategorie = new JocCategorie();

				// get the parameters selected in the table
				long joc_categorie_id = rs.getLong("joc_categorie_id");
				String nume = rs.getString("nume");
				String descriere = rs.getString("descriere");

				// add to list
				jocCategorie.setJoc_categorie_id(joc_categorie_id);
				jocCategorie.setNume(nume);
				jocCategorie.setDescriere(descriere);

				return jocCategorie;
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
	public List<JocCategorie> getAllCategory(){
		String sql = "Select * from joc_categorie";
		List<JocCategorie> categoryList = new ArrayList<JocCategorie>();

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			ResultSet rs = pstm.executeQuery();

			while (rs.next()) {
				JocCategorie jocCategorie = new JocCategorie();

				// get the parameters selected in the table
				long joc_categorie_id = rs.getLong("joc_categorie_id");
				String nume = rs.getString("nume");
				String descriere = rs.getString("descriere");

				// add to list
				jocCategorie.setJoc_categorie_id(joc_categorie_id);
				jocCategorie.setNume(nume);
				jocCategorie.setDescriere(descriere);

				categoryList.add(jocCategorie);
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		// ! remember to return the new List
		return categoryList;
	}
}