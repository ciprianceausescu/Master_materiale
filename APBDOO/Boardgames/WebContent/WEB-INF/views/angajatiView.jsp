<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Angajati</title>
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
		<h3>Angajati</h3>
		<div class="col-md-12" style="display: inline">
			<div class="col-md-6">
				<a href="Angajati?action=insertForm" class="btn btn-outline-success">Inserare angajati</a>
			</div>
			<span></span>
			<div class="col-md-6">
				<form action="?action=search" method="POST">
					<div class="form-group">
						<label for="nume">Cautare dupa nume:</label> <input type="text"
							class="form-control" id="nume" name="nume"
							placeholder="nume angajat...">
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
					<th scope="col">Telefon</th>
					<th scope="col">Strada</th>
					<th scope="col">Numar</th>
					<th scope="col">Oras</th>
					<th scope="col"></th>
				</tr>
			</thead>
			<tbody>
				<c:forEach items="${employees}" var="employee">
					<tr>
						<td scope="row">#${employee.getAngajat_id()}</td>
						<td>${employee.getNume()}</td>
						<td>${employee.getTelefon()}</td>
						<td>${employee.getAdresaInfo().getStrada()}</td>
						<td>${employee.getAdresaInfo().getNumar()}</td>
						<td>${employee.getAdresaInfo().getOras()}</td>
						<td><a
							href="Angajati?action=edit&id=${employee.getAngajat_id()}"
							class="btn btn-outline-success">Edit</a> 
							<a
							href="Angajati?action=delete&id=${employee.getAngajat_id()}"
							class="btn btn-outline-success"
							onclick="return confirm('Sunteti sigur ca doriti sa stergeti angajatul?')">Delete</a></td>
					</tr>			
				</c:forEach>
			</tbody>
		</table>
	</div>
</body>
</html>