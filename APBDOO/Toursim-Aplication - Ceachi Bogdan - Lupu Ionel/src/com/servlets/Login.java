package com.servlets;

import java.io.IOException;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import com.DAO.Implementations.*;
import com.dbConnection.MySQLConnection;
import com.dbutils.DBUtils;

import java.sql.Connection;

@WebServlet("/Login")
public class Login extends HttpServlet {
	
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        RequestDispatcher dispatcher = request.getServletContext()
                .getRequestDispatcher("/WEB-INF/views/loginView.jsp");
        dispatcher.forward(request, response);
	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String userName = request.getParameter("username");
		String password = request.getParameter("password");
		
		EmployeeImpl employee = new EmployeeImpl();
		
		if(employee.checkEmployee(userName, password) == true) {
			HttpSession session = request.getSession();
			session.setAttribute("username", userName); // punem in sesiune data, username
			response.sendRedirect("Home");
		}else {
			response.sendRedirect("Login");
		}
		
		
		/*
		try {
			Connection conn = MySQLConnection.startConnection();
			if(DBUtils.checkEmployee(conn, userName, password) == true) {
				
				HttpSession session = request.getSession();
				session.setAttribute("username", userName); // punem in sesiune data, username
				response.sendRedirect("homeView.jsp");
				
			}else {
				response.sendRedirect("loginView.jsp");
			}
		}catch(Exception e) {
			e.printStackTrace();
		}
		*/
	}

}
