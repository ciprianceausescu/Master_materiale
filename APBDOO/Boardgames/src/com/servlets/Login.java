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
		String username = request.getParameter("username");
		String parola = request.getParameter("parola");
		
		UtilizatorImpl utilizator = new UtilizatorImpl();
		
		if(utilizator.checkUser(username, parola) == true) {
			HttpSession session = request.getSession();
			session.setAttribute("username", username); // punem in sesiune data, username
			response.sendRedirect("Home");
		}else {
			response.sendRedirect("Login");
		}
	}

}
