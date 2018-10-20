package com.dbConnection;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

public class MySQLConnection {
	
	private static Connection connection;
	
	public static Connection startConnection() throws Exception {
		
		if(connection != null)
			return connection;
		
		String databaseName = "tourism_agency";
		String url="jdbc:mysql://localhost:3306/"+databaseName;
		String username="root";
		String password="1234";
		Class.forName("com.mysql.jdbc.Driver");
		
		return connection = DriverManager.getConnection(url,username,password);
	}
	
	
	public static Connection startConnection (String hostName, String databaseName, 
											  String userName, String password) throws Exception {
		
		 String url = "jdbc:mysql://" + hostName + ":3306/" + databaseName;
		Class.forName("com.mysql.jdbc.Driver");
		Connection con=DriverManager.getConnection(url,userName,password);
		
		return con;
	}
	
	public static void findCountry(Connection conn) throws SQLException {
        String sql = "Select * from country ";
 
        PreparedStatement pstm = conn.prepareStatement(sql);
 
        ResultSet rs = pstm.executeQuery();
 
        while (rs.next()) {
            
            System.out.println(rs.getString("name"));
        }
        return;
    }

}
