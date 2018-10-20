/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package baza_de_date_simpla;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;


public class Baza_de_date_simpla {
    
    public static void Afisare(Connection conn)
    {
          
        Statement stmt;
        try {
            stmt = conn.createStatement();
            
            
            ResultSet rs = stmt.executeQuery("SELECT * FROM date");

        while (rs.next())
        {
            String cnp = rs.getString("CNP");
            String nume = rs.getString("Nume");
            double salariu = rs.getDouble("Salariu");
            int varsta = rs.getInt("Varsta");

            System.out.println(cnp + "," + nume + "," + varsta + "," + salariu);
        }
         
        } catch (SQLException ex) {
            Logger.getLogger(Baza_de_date_simpla.class.getName()).log(Level.SEVERE, null, ex);
        }

          
    }
    
    public static void main(String[] args)  {
        try {
            Class.forName("com.mysql.jdbc.Driver");
        } catch (ClassNotFoundException ex) {
            Logger.getLogger(Baza_de_date_simpla.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        Connection conn;
        try {
            conn = DriverManager.getConnection("jdbc:mysql://localhost/angajati", "root", "12345");
            
        Afisare(conn);
        
        Statement stmt = conn.createStatement();
        //stmt.executeUpdate("INSERT INTO date(CNP, Nume, Varsta, Salariu) VALUES ('1334567','Vasilescu Ioana',47,2700.50)");
        Afisare(conn);
        //afisati datelel persoanelor care au sal mai mare decat x
        String sqlparam="SELECT * FROM date WHERE salariu >= ?";
        PreparedStatement ps=conn.prepareStatement(sqlparam);
        
        double x;
        String r=JOptionPane.showInputDialog("Dati salariul minim");
        x=Double.parseDouble(r);
        
        ps.setDouble(1, x);
        
        ResultSet rez=ps.executeQuery();
        
        System.out.println("\n");;
        
        while(rez.next())
        {
            System.out.println(rez.getString("Nume")+" "+rez.getDouble("Salariu"));
        }
         
     /*
        stmt.executeUpdate("DELETE FROM test_java WHERE nume = 'Vasilescu Ioana'");

        System.out.println();

        rs = stmt.executeQuery("SELECT * FROM test_java");

        while (rs.next())
        {
            int id = rs.getInt("id");
            String nume = rs.getString("nume");
            int salariu = rs.getInt("salariu");
            int varsta = rs.getInt("varsta");

            System.out.println(id + "," + nume + "," + varsta + "," + salariu);
        }
   */
        //rs.close();
        stmt.close();
        conn.close();
            
        } catch (SQLException ex) {
            Logger.getLogger(Baza_de_date_simpla.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        
        
    }
}
        
    

