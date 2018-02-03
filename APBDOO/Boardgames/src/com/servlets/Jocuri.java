package com.servlets;

import java.io.IOException;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import com.DAO.Implementations.AngajatImpl;
import com.DAO.Implementations.JocCategorieImpl;
import com.DAO.Implementations.JocImpl;
import com.DAO.Implementations.JocNrJucatoriImpl;
import com.DAO.Implementations.JocProducatorImpl;
import com.DAO.Implementations.JocTipImpl;
import com.DAO.Implementations.Librarie;
import com.Tables.Angajat;
import com.Tables.Joc;
import com.Tables.JocCategorie;
import com.Tables.JocNrJucatori;
import com.Tables.JocProducator;
import com.Tables.JocTip;
import com.mysql.jdbc.StringUtils;

/**
 * Servlet implementation class Jocuri
 */
@WebServlet("/Jocuri")
public class Jocuri extends HttpServlet {
	private static final long serialVersionUID = 1L;

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doGet(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {
		String action = request.getParameter("action");

		if (action == null)
			action = "";
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
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doPost(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}

	private void edit(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		String idParam = request.getParameter("id");

		if (idParam != null) {
			int id = Integer.parseInt(idParam);
			JocImpl ji = new JocImpl();
			JocProducatorImpl jpi = new JocProducatorImpl();
			JocCategorieImpl jci = new JocCategorieImpl();
			JocTipImpl jti = new JocTipImpl();
			JocNrJucatoriImpl jnji = new JocNrJucatoriImpl();
			Joc game = ji.getGame(id);
			List<JocProducator> producerList = jpi.getAllProducer();
			List<JocCategorie> categoryList = jci.getAllCategory();
			List<JocTip> gameTypeList = jti.getAllTypes();
			List<JocNrJucatori> gameNumberPlayersList = jnji.getAllNumberPlayers();
			request.setAttribute("game", game);
			request.setAttribute("producers", producerList);
			request.setAttribute("categories", categoryList);
			request.setAttribute("types", gameTypeList);
			request.setAttribute("players", gameNumberPlayersList);
		}

		RequestDispatcher dispatcher = request
				.getRequestDispatcher("/WEB-INF/views/jocuri/edit.jsp");
		dispatcher.forward(request, response);
	}

	private void list(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		HttpSession session = request.getSession(false);
		session.removeAttribute("updated");
		JocImpl ji = new JocImpl();
		List<Joc> gameList = ji.getAllGames();

		request.setAttribute("games", gameList);

		RequestDispatcher dispatcher = request.getServletContext()
				.getRequestDispatcher("/WEB-INF/views/jocuriView.jsp");
		dispatcher.forward(request, response);
	}

	private void update(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		HttpSession session = request.getSession(false);
		String idParam = request.getParameter("id");
		int id = Integer.parseInt(idParam);
		String nume = request.getParameter("nume");
		String descriere = request.getParameter("descriere");
		String joc_producator_id = request.getParameter("producator");
		int joc_producator_id_int = Integer.parseInt(joc_producator_id);
		String joc_categorie_id = request.getParameter("categorie");
		int joc_categorie_id_int = Integer.parseInt(joc_categorie_id);
		String joc_tip_id = request.getParameter("tip");
		int joc_tip_id_int = Integer.parseInt(joc_tip_id);
		String joc_numar_jucatori_id = request.getParameter("jucatori");
		int joc_numar_jucatori_id_int = Integer.parseInt(joc_numar_jucatori_id);
		
		Joc joc = Librarie.getJocImpl().getGame(id);
		joc.setNume(nume);
		joc.setDescriere(descriere);
		joc.setJoc_producator_id(joc_producator_id_int);
		joc.setJoc_categorie_id(joc_categorie_id_int);
		joc.setJoc_tip_id(joc_tip_id_int);
		joc.setJoc_numar_jucatori_id(joc_numar_jucatori_id_int);
		Librarie.getJocImpl().updateGame(joc);
		session.setAttribute("updated", "updated");
		response.sendRedirect("?action=edit&id=" + id);
	}
	private void search(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		List<Joc> gameList;
		String nume = request.getParameter("nume");
		if(nume.length()>0){
			gameList = Librarie.getJocImpl().searchGame(nume);
		}
		else{
			gameList = Librarie.getJocImpl().getAllGames();
		}
		
		request.setAttribute("games", gameList);

		RequestDispatcher dispatcher = request.getServletContext()
				.getRequestDispatcher("/WEB-INF/views/jocuriView.jsp");
		dispatcher.forward(request, response);
	}
	private void delete(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		String idParam = request.getParameter("id");
		int id = Integer.parseInt(idParam);
		Librarie.getJocImpl().deleteGame(id);
		
		JocImpl ji = new JocImpl();
		List<Joc> gameList = ji.getAllGames();

		request.setAttribute("games", gameList);
		
		RequestDispatcher dispatcher = request.getServletContext()
				.getRequestDispatcher("/WEB-INF/views/jocuriView.jsp");
		dispatcher.forward(request, response);
	}
	private void insertForm(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		JocProducatorImpl jpi = new JocProducatorImpl();
		JocCategorieImpl jci = new JocCategorieImpl();
		JocTipImpl jti = new JocTipImpl();
		JocNrJucatoriImpl jnji = new JocNrJucatoriImpl();
		List<JocProducator> producerList = jpi.getAllProducer();
		List<JocCategorie> categoryList = jci.getAllCategory();
		List<JocTip> gameTypeList = jti.getAllTypes();
		List<JocNrJucatori> gameNumberPlayersList = jnji.getAllNumberPlayers();
		request.setAttribute("producers", producerList);
		request.setAttribute("categories", categoryList);
		request.setAttribute("types", gameTypeList);
		request.setAttribute("players", gameNumberPlayersList);
	
		RequestDispatcher dispatcher = request.getServletContext()
				.getRequestDispatcher("/WEB-INF/views/jocuri/insert.jsp");
		dispatcher.forward(request, response);
	}
	private void insert(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		Joc game = new Joc();
		String nume = request.getParameter("nume");
		game.setNume(nume);
		String descriere = request.getParameter("descriere");
		game.setDescriere(descriere);
		String imagine = request.getParameter("imagine");
		game.setImagine(imagine);
		long an = Long.parseLong(request.getParameter("an")); 
		game.setAn_publicare(an);
		long categorie = Long.parseLong(request.getParameter("categorie")); 
		game.setJoc_categorie_id(categorie);
		long producator = Long.parseLong(request.getParameter("producator")); 
		game.setJoc_producator_id(producator);
		long tip = Long.parseLong(request.getParameter("tip")); 
		game.setJoc_tip_id(tip);
		long jucatori = Long.parseLong(request.getParameter("jucatori")); 
		game.setJoc_numar_jucatori_id(jucatori);
		
		JocImpl gi = new JocImpl();
		gi.addGame(game);
		
		List<Joc> gameList = gi.getAllGames();

		request.setAttribute("games", gameList);

		RequestDispatcher dispatcher = request.getServletContext()
				.getRequestDispatcher("/WEB-INF/views/jocuriView.jsp");
		dispatcher.forward(request, response);
	}
}
