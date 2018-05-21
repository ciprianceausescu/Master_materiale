package com.company.shop.controlers;

import java.io.IOException;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.company.shop.model.ProductDto;
import com.company.shop.model.UserDto;
import com.company.shop.services.ProductService;
import com.company.shop.services.UserService;

@WebServlet("/home")
public class HomeController extends HttpServlet {
	
	private static final long serialVersionUID = 1L;
	private static final String HOME_VIEW = "home.jsp";
	
	UserService userService = new UserService();
	
	ProductService productService = new ProductService();
       
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// Populate user data
		UserDto user = userService.getCurrentLoggedInUser();
		request.setAttribute("user", user);
		
		// Populate products data
		List<ProductDto> availableProducts = productService.getListAvailableProducts();
		request.setAttribute("products", availableProducts);
		
		// Forward to home page
		RequestDispatcher dispatcher = request.getRequestDispatcher(HOME_VIEW);
		dispatcher.forward(request, response);
	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO
	}

}
