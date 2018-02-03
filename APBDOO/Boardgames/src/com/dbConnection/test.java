package com.dbConnection;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

public class test {
	public static void findAddress() throws Exception {
		int size = 0;
		//MySQLConnection mysql = new MySQLConnection();
		Connection conn = MySQLConnection.startConnection();
		/*String sql = "Select * from adresa where adresa_id = 60";
		PreparedStatement pstm = conn.prepareStatement(sql);
		ResultSet rs = pstm.executeQuery();*/
		String sql = "Select * from utilizator where username=? and parola=?";
		PreparedStatement pstm = conn.prepareStatement(sql);
		pstm.setString(1, "cip");
		pstm.setString(2, "cipa");
		ResultSet rs = pstm.executeQuery();
		while (rs.next()) {
		    size++;
		}
		if(size==1)
			System.out.print("Da");
		else 
			System.out.print("Nu");
		/*while (rs.next()) {
			System.out.println(rs.getString("strada"));
		}*/
		return;
	}
	public static void main(String[] args) throws Exception {
		findAddress();
	}

}
