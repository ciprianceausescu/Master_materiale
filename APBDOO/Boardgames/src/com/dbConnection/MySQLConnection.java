package com.dbConnection;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class MySQLConnection {
	
	private static Connection connection;
	
	public static Connection startConnection() throws Exception {
		if(connection != null){
			System.out.println("aici!");
			return connection;
		}
		String url="jdbc:oracle:thin:@localhost:1521:xe";
		String username="apbdoo";
		String password="apbdoo";
		Class.forName("oracle.jdbc.driver.OracleDriver");
		return connection = DriverManager.getConnection(url,username,password);
	}
	
	public static Connection startConnection (String hostName, String databaseName, 
											  String userName, String password) throws Exception {
		String url = "jdbc:oracle:thin:@localhost:1521:xe";
		Class.forName("com.mysql.jdbc.Driver");
		Connection con=DriverManager.getConnection(url,userName,password);
		return con;
	}

}

/*public class MySQLConnection {
	private Connection connection = null;
	static final String oracleURL = "jdbc:oracle:thin:@localhost:1521:xe";
	static final String oracleUsername = "apbdoo";
	static final String oraclePassword = "apbdoo";

	public MySQLConnection() throws SQLException {
		System.out.println("Connection method");
		try {
			Class.forName("oracle.jdbc.driver.OracleDriver");
		} catch (ClassNotFoundException e) {
			System.out.println("Where is your Oracle JDBC Driver?");
			e.printStackTrace();
			return;
		}
		System.out.println("Oracle JDBC Driver Registered!");
		connection = null;

		try {
			connection = DriverManager.getConnection(oracleURL, oracleUsername,
					oraclePassword);
		} catch (SQLException e) {
			System.out.println("Failed to connect. Check output console");
			e.printStackTrace();
			return;
		}

		if (connection != null) {
			System.out.println("Connection successful");
		} else {
			System.out.println("Failed to connect");
		}
	}
	public Connection startConnection(){
		return connection;
	}
}*/
