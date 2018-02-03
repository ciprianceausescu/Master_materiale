<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Jocuri</title>
</head>
<body>
	<link rel="stylesheet"
		href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css" />

	<jsp:include page="_menu.jsp"></jsp:include>

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
		<h3>Jocuri</h3>
		<div class="col-md-12" style="display: inline">
			<div class="col-md-6">
				<a href="Jocuri?action=insertForm" class="btn btn-outline-success">Inserare jocuri</a>
			</div>
			<span></span>
			<div class="col-md-6">
				<form action="?action=search" method="POST">
					<div class="form-group">
						<label for="nume">Cautare dupa nume:</label> <input type="text"
							class="form-control" id="nume" name="nume"
							placeholder="nume joc...">
					</div>
					<button type="submit" class="btn btn-default">Cautare</button>
				</form>
			</div>
		</div>
		<br>
		<table class="table table-bordered">
			<thead class="thead-inverse">
				<tr>
					<th scope="col">#</th>
					<th scope="col">Nume</th>
					<th scope="col">Poza</th>
					<th scope="col">Descriere</th>
					<th scope="col">Categorie</th>
					<th scope="col">Producator</th>
					<th scope="col">Tip</th>
					<th scope="col"># juc</th>
					<th scope="col"></th>
				</tr>
			</thead>
			<tbody>
				<c:forEach items="${games}" var="game">
					<tr>
						<td scope="row">#${game.getJoc_id()}</td>
						<td>${game.getNume()}</td>
						<td><img src = ${ game.getImagine()} width="100" height="100"></td>
						<td>${game.getDescriere()}</td>
						<td>${game.getJoc_categorie().getNume()}</td>
						<td>${game.getJoc_producator().getNume()}</td>
						<td>${game.getJoc_tip().getNume()}</td>
						<td>${game.getJocNrJucatori().getValoare()}</td>
						<td><a href="Jocuri?action=edit&id=${game.getJoc_id()}"
							class="btn btn-outline-success">Edit</a> 
							<a href="Jocuri?action=delete&id=${game.getJoc_id()}"
							class="btn btn-outline-success" 
							onclick="return confirm('Sunteti sigur ca doriti sa stergeti jocul?')">Delete</a></td>
					</tr>
				</c:forEach>
			</tbody>
		</table>
	</div>
</body>
</html>