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
import javax.servlet.http.HttpSession;

import com.DAO.Implementations.AngajatImpl;
import com.DAO.Implementations.Librarie;
import com.Tables.Angajat;
import com.Tables.Joc;
import com.mysql.jdbc.StringUtils;

/**
 * Servlet implementation class Employees
 */
@WebServlet("/Angajati")
public class Angajati extends HttpServlet {
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String action = request.getParameter("action");
		
		if(action == null)action = "";
        switch (action) {
            case "edit":
                edit(request, response);
                break;
            case "update":
            	update(request, response);
            	break;
            case "search":
    			search(request, response);
    			break;
            case "delete":
    			delete(request, response);
    			break;
            case "insertForm":
            	insertForm(request, response);
            	break;
            case "insert":
            	insert(request, response);
            	break;
            default:
            	list(request, response);
            	break;
        }
	}
	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
	
	private void edit(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String idParam = request.getParameter("id");
		
		if( idParam != null) {
			int id = Integer.parseInt(idParam);
			AngajatImpl ei = new AngajatImpl();
			Angajat employee = ei.getEmployee(id);
	        request.setAttribute("employee", employee);
        }
        
		RequestDispatcher dispatcher = request.getRequestDispatcher("/WEB-INF/views/angajati/edit.jsp");
        dispatcher.forward(request, response);
	}
	private void list(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		AngajatImpl ei = new AngajatImpl();
		List<Angajat> employees = ei.getAllEmployees();
		
		request.setAttribute("employees", employees);

        RequestDispatcher dispatcher = request.getServletContext()
                .getRequestDispatcher("/WEB-INF/views/angajatiView.jsp");
        dispatcher.forward(request, response);
	}
	private void update(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		HttpSession session = request.getSession(false);
		String idParam = request.getParameter("id");
		/*
		 * if( StringUtils.isNullOrEmpty(idParam) ) { insert(request,response);
		 * return; }
		 */
		int id = Integer.parseInt(idParam);
		String nume = request.getParameter("nume");
		String telefonString = request.getParameter("telefon");
		long telefon = Long.parseLong(telefonString);

		Angajat angajat = Librarie.getAngajatImpl().getEmployee(id);
		angajat.setNume(nume);
		angajat.setTelefon(telefon);
		Librarie.getAngajatImpl().updateEmployee(angajat);
		session.setAttribute("updated", "updated");
		response.sendRedirect("?action=edit&id=" + id);
	}
	private void search(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		List<Angajat> employeeList;
		String nume = request.getParameter("nume");
		if(nume.length()>0){
			employeeList = Librarie.getAngajatImpl().searchEmployee(nume);
		}
		else{
			employeeList = Librarie.getAngajatImpl().getAllEmployees();
		}
		
		request.setAttribute("employees", employeeList);

		RequestDispatcher dispatcher = request.getServletContext()
				.getRequestDispatcher("/WEB-INF/views/angajatiView.jsp");
		dispatcher.forward(request, response);
	}
	private void delete(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		String idParam = request.getParameter("id");
		int id = Integer.parseInt(idParam);
		Librarie.getAngajatImpl().deleteEmployee(id);
		
		AngajatImpl ei = new AngajatImpl();
		List<Angajat> employees = ei.getAllEmployees();
		
		request.setAttribute("employees", employees);
		
		RequestDispatcher dispatcher = request.getServletContext()
				.getRequestDispatcher("/WEB-INF/views/angajatiView.jsp");
		dispatcher.forward(request, response);
	}
	private void insertForm(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		RequestDispatcher dispatcher = request.getServletContext()
				.getRequestDispatcher("/WEB-INF/views/angajati/insert.jsp");
		dispatcher.forward(request, response);
	}
	private void insert(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		Angajat employee = new Angajat();
		String nume = request.getParameter("nume");
		employee.setNume(nume);
		long cnp = Long.parseLong(request.getParameter("cnp")); 
		employee.setCNP(cnp);
		int adresa = Integer.parseInt(request.getParameter("adresa")); 
		employee.setAdresa(adresa);
		long telefon = Long.parseLong(request.getParameter("telefon")); 
		employee.setTelefon(telefon);
		int departament = Integer.parseInt(request.getParameter("departament"));
		employee.setMagazin_departament_id(departament);
		int contract = Integer.parseInt(request.getParameter("contract")); 
		employee.setAngajat_contract_id(contract);
		int pozitie = Integer.parseInt(request.getParameter("pozitie")); 
		employee.setAngajat_pozitie_id(pozitie);
		
		AngajatImpl ei = new AngajatImpl();
		ei.addEmployee(employee);

		List<Angajat> employees = ei.getAllEmployees();
		
		request.setAttribute("employees", employees);

        RequestDispatcher dispatcher = request.getServletContext()
                .getRequestDispatcher("/WEB-INF/views/angajatiView.jsp");
        dispatcher.forward(request, response);
	}
}
