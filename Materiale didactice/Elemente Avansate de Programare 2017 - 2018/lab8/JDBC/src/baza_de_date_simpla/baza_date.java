/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package baza_de_date_simpla;


import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Level;
import java.util.logging.Logger;

public class baza_date {

    
    public static void main(String[] args) {
        try {
            Class.forName("com.mysql.jdbc.Driver");
        
        } catch (ClassNotFoundException ex) {
            
        }
        try {
            Connection conn=DriverManager.getConnection("jdbc:mysql//localhost/angajati", "root","12345");
            Statement st=conn.createStatement();
            
            String sql="SELECT * FROM date";
            ResultSet rez=st.executeQuery(sql);
            
            while(rez.next())
            {
                String cnp=rez.getString("CNP");
                String nume=rez.getString("Nume");
                int varsta=rez.getInt("Varsta");
                double salariu=rez.getDouble("Salariu");
                
                System.out.println(cnp+" "+" "+nume+" "+varsta+" "+salariu);
            }
   
        } catch (SQLException ex) {
           
        }
        
        
    }
}

