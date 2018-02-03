package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.JocProducatorDAO;
import com.Tables.Joc;
import com.Tables.JocProducator;
import com.dbConnection.MySQLConnection;

public class JocProducatorImpl implements JocProducatorDAO{
	public JocProducator getGameProducer(long id){
		String sql = "Select * from joc_producator where joc_producator_id=?";

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setLong(1, id);
			ResultSet rs = pstm.executeQuery();

			if (rs.next()) {
				JocProducator jocProducator = new JocProducator();

				// get the parameters selected in the table
				long joc_producator_id = rs.getLong("joc_producator_id");
				String nume = rs.getString("nume");
				String tara = rs.getString("tara");

				// add to list
				jocProducator.setJoc_producator_id(joc_producator_id);
				jocProducator.setNume(nume);
				jocProducator.setTara(tara);

				return jocProducator;
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
	public List<JocProducator> getAllProducer(){
		String sql = "Select * from joc_producator";
		List<JocProducator> producerList = new ArrayList<JocProducator>();

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			ResultSet rs = pstm.executeQuery();

			while (rs.next()) {
				JocProducator jocProducator = new JocProducator();

				// get the parameters selected in the table
				long joc_producator_id = rs.getLong("joc_producator_id");
				String nume = rs.getString("nume");
				String tara = rs.getString("tara");

				// add to list
				jocProducator.setJoc_producator_id(joc_producator_id);
				jocProducator.setNume(nume);
				jocProducator.setTara(tara);

				producerList.add(jocProducator);
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		// ! remember to return the new List
		return producerList;
	}
}