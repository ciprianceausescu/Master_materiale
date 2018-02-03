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
				<label for="imagine">Imagine:</label>
				<textarea class="form-control" rows="5" id="imagine"
					name="imagine"></textarea>
			</div>
			<div class="form-group">
				<label for="descriere">Descriere:</label>
				<textarea class="form-control" rows="5" id="descriere"
					name="descriere"></textarea>
			</div>
			<div class="form-group">
				<label for="an">An publicare:</label> 
				<input type="text"
					class="form-control" id="an" name="an">
			</div>
			<div class="form-group">
				<label for="categorie">Categorie:</label> 
				<select name="categorie" id="categorie" size="1">
				<c:forEach items="${categories}" var="category">
					<option value="${category.getJoc_categorie_id()}">${category.getNume()}</option>
				</c:forEach>
				</select>
			</div>
			<div class="form-group">
				<label for="producator">Producator:</label> 
				<select name="producator" id="producator" size="1">
				<c:forEach items="${producers}" var="producer">
					<option value="${producer.getJoc_producator_id()}">${producer.getNume()}</option>
				</c:forEach>
				</select>
			</div>
			<div class="form-group">
				<label for="tip">Tip:</label> 
				<select name="tip" id="tip" size="1">
         		<c:forEach items="${types}" var="type">
					<option value="${type.getJoc_tip_id()}">${type.getNume()}</option>
				</c:forEach>
				</select>
			</div>
			<div class="form-group">
				<label for="jucatori">Numar jucatori:</label> 
				<select name="jucatori" id="jucatori" size="1">
         		<c:forEach items="${players}" var="player">
					<option value="${player.getJoc_numar_jucatori_id()}">${player.getValoare()}</option>
				</c:forEach>
				</select>
			</div>
			<button type="submit" class="btn btn-default">Salvare</button>
		</form>
	</div>
</body>
</html>