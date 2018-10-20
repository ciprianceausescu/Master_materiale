package com.DAO.Implementations;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import com.DaoInterfaces.ContactDAO;
import com.Tables.Accomodation;
import com.Tables.Accomodation_type;
import com.Tables.Contact;
import com.dbConnection.MySQLConnection;

public class ContactImpl implements ContactDAO {

	@Override
	public List<Contact> getAllContacts() {
		String sql = "Select * from contact";
		List<Contact> contactList = new ArrayList<Contact>();
				
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
	        ResultSet rs = pstm.executeQuery();
	        
	        while(rs.next()) {
	        	Contact contact = new Contact();
	        	
	        	// get the parameters selected in the table
	        	int contactID = rs.getInt("id");
	        	String contactEmail = rs.getString("email");
	        	String contactPhone = rs.getString("phone");
	        	String contactPhone2 = rs.getString("phone2");
	        	String contactFax = rs.getString("fax");
	        	String contactWebsite = rs.getString("website");
	        	String contactFacebook = rs.getString("facebook");
	        	String contactLinkedin = rs.getString("linkedin");
	        	
	        	// add to list
	        	contact.setId(contactID);
	        	contact.setEmail(contactEmail);
	        	contact.setPhone(contactPhone);
	        	contact.setPhone2(contactPhone2);
	        	contact.setFax(contactFax);
	        	contact.setWebsite(contactWebsite);
	        	contact.setFacebook(contactFacebook);
	        	contact.setLinkedin(contactLinkedin);
	        	
	        	
	        	// add to list
	        	contactList.add(contact);
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		// ! remember to return the new List
		return contactList;
	}

	@Override
	public Contact getContact(int id) {
		String sql = "Select * from contact where id=?";
		
		try {
			Connection conn = MySQLConnection.startConnection();
			
			PreparedStatement pstm = conn.prepareStatement(sql);
			pstm.setInt(1, id);
	        ResultSet rs = pstm.executeQuery();
	        
	        if(rs.next()) {
	        	Contact contact = new Contact();
	        	
	        	// get the parameters selected in the table
	        	int contactID = rs.getInt("id");
	        	String contactEmail = rs.getString("email");
	        	String contactPhone = rs.getString("phone");
	        	String contactPhone2 = rs.getString("phone2");
	        	String contactFax = rs.getString("fax");
	        	String contactWebsite = rs.getString("website");
	        	String contactFacebook = rs.getString("facebook");
	        	String contactLinkedin = rs.getString("linkedin");
	        	
	        	// add to list
	        	contact.setId(contactID);
	        	contact.setEmail(contactEmail);
	        	contact.setPhone(contactPhone);
	        	contact.setPhone2(contactPhone2);
	        	contact.setFax(contactFax);
	        	contact.setWebsite(contactWebsite);
	        	contact.setFacebook(contactFacebook);
	        	contact.setLinkedin(contactLinkedin);
	        	
	        	// return
	        	return contact;
	        }
	        pstm.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public void updateContact(Contact contact) {
		String sql = "Update contact set email=?, "
				+ "phone=?, phone2=?, fax=?, website=?, "
				+ "facebook=?, linkedin=? where id=? ";
		 
		try {
		Connection conn = MySQLConnection.startConnection();
        PreparedStatement pstm = conn.prepareStatement(sql);
 
        // implement ? in query
        pstm.setString(1, contact.getEmail());
        pstm.setString(2, contact.getPhone());
        pstm.setString(3, contact.getPhone2());
        pstm.setString(4, contact.getFax());
        pstm.setString(5, contact.getWebsite());
        pstm.setString(6, contact.getFacebook());
        pstm.setString(7, contact.getLinkedin());
        
        //execute
        pstm.executeUpdate();
        pstm.close();
		}catch(Exception e) {
			e.printStackTrace();
		}
		
	}

	@Override
	public void deleteContact(Contact contact) {
		String sql = "Delete From Contact where id= ?";
		 
		try {
			Connection conn = MySQLConnection.startConnection();
	        PreparedStatement pstm = conn.prepareStatement(sql);
	        
	        pstm.setInt(1, contact.getId());
	        pstm.executeUpdate();
		}catch(Exception e) {
			e.printStackTrace();
		}
		
	}
	

}
