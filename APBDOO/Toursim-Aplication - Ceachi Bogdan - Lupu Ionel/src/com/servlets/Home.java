package com.servlets;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.DAO.Implementations.*;
import com.Tables.*;

/**
 * Servlet implementation class Home
 */
@WebServlet("/Home")
public class Home extends HttpServlet {
	
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		Country country = Factory.getCountryImpl().getCountry("Romania");
		List<Region> regionList = new ArrayList<Region>();
		regionList = Factory.getCountryImpl().getAllCountryRegions(country);
		
		List<Accomodation> accomodations = new ArrayList<Accomodation>();
		accomodations = Factory.getAccomodationImpl().getAllAccomodations();
		
		if(accomodations != null) {
			for(Accomodation i : accomodations) {
				System.out.println(i.toString());
			}
		}
		
		Employee employee = Factory.getEmployeeImpl().getEmployee(1);
		employee.setPassword("123");
		Factory.getEmployeeImpl().updateEmployee(employee);
		

		request.setAttribute("regionList", regionList);
		request.setAttribute("accomodations", accomodations);

        RequestDispatcher dispatcher = request.getServletContext()
                .getRequestDispatcher("/WEB-INF/views/homeView.jsp");
        dispatcher.forward(request, response);
		//response.sendRedirect("homeView.jsp");
	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}

}
