package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.JocNrJucatoriDAO;
import com.Tables.JocNrJucatori;
import com.dbConnection.MySQLConnection;

public class JocNrJucatoriImpl implements JocNrJucatoriDAO{

	@Override
	public List<JocNrJucatori> getAllNumberPlayers() {
		String sql = "Select * from joc_numar_jucatori order by joc_numar_jucatori_id asc";
		List<JocNrJucatori> gameNumberList = new ArrayList<JocNrJucatori>();

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			ResultSet rs = pstm.executeQuery();

			while (rs.next()) {
				JocNrJucatori jocNrJucatori = new JocNrJucatori();

				// get the parameters selected in the table
				int joc_numar_jucatori_id = rs.getInt("joc_numar_jucatori_id");
				String valoare = rs.getString("valoare");
				String descriere = rs.getString("descriere");
				
				// add to list
				jocNrJucatori.setJoc_numar_jucatori_id(joc_numar_jucatori_id);
				jocNrJucatori.setValoare(valoare);
				jocNrJucatori.setDescriere(descriere);
				
				// add to list
				gameNumberList.add(jocNrJucatori);
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		// ! remember to return the new List
		return gameNumberList;
	}

	@Override
	public JocNrJucatori getNumberPlayers(long id) {
		String sql = "Select * from joc_numar_jucatori where joc_numar_jucatori_id=?";

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setLong(1, id);
			ResultSet rs = pstm.executeQuery();

			if (rs.next()) {
				JocNrJucatori jocNrJucatori = new JocNrJucatori();

				// get the parameters selected in the table
				int joc_numar_jucatori_id = rs.getInt("joc_numar_jucatori_id");
				String valoare = rs.getString("valoare");
				String descriere = rs.getString("descriere");

				// add to list
				jocNrJucatori.setJoc_numar_jucatori_id(joc_numar_jucatori_id);
				jocNrJucatori.setValoare(valoare);
				jocNrJucatori.setDescriere(descriere);
				
				return jocNrJucatori;
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		return null;
	}

}
