package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

import com.DaoInterfaces.UtilizatorDAO;
import com.Tables.Angajat;
import com.dbConnection.MySQLConnection;

public class UtilizatorImpl implements UtilizatorDAO {
	public boolean checkUser(String username, String parola) {
		String sql = "Select * from utilizator where username=? and parola=?";
		try {
			int size = 0;
			Connection conn = MySQLConnection.startConnection();
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setString(1, username);
			pstm.setString(2, parola);
			ResultSet rs = pstm.executeQuery();
			while (rs.next()) {
			    size++;
			}
			pstm.close();
			rs.close();
			if (size == 1)
				return true;
			else
				return false;

		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}
}
