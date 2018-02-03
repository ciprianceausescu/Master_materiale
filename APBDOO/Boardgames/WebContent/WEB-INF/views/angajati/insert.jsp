<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Jocuri</title>

<link rel="stylesheet"
	href="<c:url value="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css" />" />

</head>
<body>
	<jsp:include page="../_menu.jsp"></jsp:include>

	<%
		// spunei browserului sa nu memoreze nimic in cache, sa rezolvi problema cu back
		response.setHeader("Cache-Control",
				"no-cache, no-store, must-revalidate");
		response.setHeader("Pragma", "no-cache"); // same thing, but for HTTP 1.0
		response.setHeader("Expires", "0");//Proxies
		if (session.getAttribute("username") == null) {
			response.sendRedirect("loginView.jsp");
		}
	%>

	<div class="container">
		<form action="?action=insert" method="POST">
			<div class="form-group">
				<label for="nume">Nume:</label> 
				<input type="text"
					class="form-control" id="nume" name="nume">
			</div>
			<div class="form-group">
				<label for="cnp">CNP:</label> 
				<input type="text"
					class="form-control" id="cnp" name="cnp">
			</div>
			<div class="form-group">
				<label for="adresa">Adresa:</label> 
				<input type="text"
					class="form-control" id="adresa" name="adresa">
			</div>
			<div class="form-group">
				<label for="telefon">Telefon:</label> 
				<input type="text"
					class="form-control" id="telefon" name="telefon">
			</div>
			<div class="form-group">
				<label for="departament">Departament:</label> 
				<input type="text"
					class="form-control" id="departament" name="departament">
			</div>
			<div class="form-group">
				<label for="contract">Contract:</label> 
				<input type="text"
					class="form-control" id="contract" name="contract">
			</div>
			<div class="form-group">
				<label for="pozitie">Pozitie:</label> 
				<input type="text"
					class="form-control" id="pozitie" name="pozitie">
			</div>
			<button type="submit" class="btn btn-default">Salvare</button>
		</form>
	</div>
</body>
</html>