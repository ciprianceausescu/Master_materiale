package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.JocTipDAO;
import com.Tables.Angajat;
import com.Tables.JocTip;
import com.dbConnection.MySQLConnection;

public class JocTipImpl implements JocTipDAO{

	@Override
	public List<JocTip> getAllTypes() {
		String sql = "Select * from joc_tip order by joc_tip_id asc";
		List<JocTip> gameTypeList = new ArrayList<JocTip>();

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			ResultSet rs = pstm.executeQuery();

			while (rs.next()) {
				JocTip jocTip = new JocTip();

				// get the parameters selected in the table
				int joc_tip_id = rs.getInt("joc_tip_id");
				String nume = rs.getString("nume");
				String descriere = rs.getString("descriere");
				
				// add to list
				jocTip.setJoc_tip_id(joc_tip_id);
				jocTip.setNume(nume);
				jocTip.setDescriere(descriere);
				
				// add to list
				gameTypeList.add(jocTip);
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		// ! remember to return the new List
		return gameTypeList;
	}

	@Override
	public JocTip getGameType(long id) {
		String sql = "Select * from joc_tip where joc_tip_id=?";

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setLong(1, id);
			ResultSet rs = pstm.executeQuery();

			if (rs.next()) {
				JocTip jocTip = new JocTip();

				// get the parameters selected in the table
				int joc_tip_id = rs.getInt("joc_tip_id");
				String nume = rs.getString("nume");
				String descriere = rs.getString("descriere");

				// add to list
				jocTip.setJoc_tip_id(joc_tip_id);
				jocTip.setNume(nume);
				jocTip.setDescriere(descriere);
				
				return jocTip;
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
