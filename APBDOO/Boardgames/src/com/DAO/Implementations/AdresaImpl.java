package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.AdresaDAO;
import com.Tables.Adresa;
import com.dbConnection.MySQLConnection;

public class AdresaImpl implements AdresaDAO{

	@Override
	public Adresa getAddress(long id) {
		String sql = "Select * from adresa where adresa_id=?";

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setLong(1, id);
			ResultSet rs = pstm.executeQuery();

			if (rs.next()) {
				Adresa adresa = new Adresa();

				// get the parameters selected in the table
				long adresa_id = rs.getLong("adresa_id");
				String strada = rs.getString("strada");
				String numar = rs.getString("numar");
				String oras = rs.getString("oras");

				// add to list
				adresa.setAdresa_id(adresa_id);
				adresa.setStrada(strada);
				adresa.setNumar(numar);
				adresa.setOras(oras);

				return adresa;
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

	@Override
	public List<Adresa> getAllAddresses() {
		String sql = "Select * from adresa";
		List<Adresa> addressList = new ArrayList<Adresa>();

		try {
			Connection conn = MySQLConnection.startConnection();
			System.out.println("Connected.");
			PreparedStatement pstm = conn.prepareStatement(sql);
			ResultSet rs = pstm.executeQuery();

			while (rs.next()) {
				Adresa adresa = new Adresa();

				// get the parameters selected in the table
				long adresa_id = rs.getLong("adresa_id");
				String strada = rs.getString("strada");
				String numar = rs.getString("numar");
				String oras = rs.getString("oras");

				// add to list
				adresa.setAdresa_id(adresa_id);
				adresa.setStrada(strada);
				adresa.setNumar(numar);
				adresa.setOras(oras);

				addressList.add(adresa);
			}
			pstm.close();
			rs.close();
		} catch (Exception e) {
			System.out.println("Not connected.");
			e.printStackTrace();
		}
		// ! remember to return the new List
		return addressList;
	}
}
