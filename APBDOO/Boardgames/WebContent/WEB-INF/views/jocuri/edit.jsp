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
		<%
			if (session.getAttribute("updated") == null) {
		%>
		<h4>Editare joc: ${ game.getNume() }</h4>
		<%
			} else {
		%>
		<h4>
			<font color="red">(*)Editare joc: ${ game.getNume() }</font>
		</h4>
		<%
			}
		%>
		<h4>Categorie: ${ game.getJoc_categorie().getNume() }</h4>
		<h4>Producator: ${ game.getJoc_producator().getNume() }</h4>
		<h4>Tip: ${ game.getJoc_tip().getNume() }</h4>
		<h4>Numar jucatori: ${ game.getJocNrJucatori().getValoare() }</h4>
		<form action="?action=update&id=${ game.getJoc_id() }" method="POST">
			<div class="form-group">
				<label for="nume">Nume:</label> 
				<input type="text"
					class="form-control" id="nume" value="${game.getNume()}"
					name="nume">
			</div>
			<div class="form-group">
				<label for="descriere">Descriere:</label>
				<textarea class="form-control" rows="5" id="descriere"
					name="descriere">${game.getDescriere()}</textarea>
			</div>
			<div class="form-group">
				<label for="categorie">Categorie:</label> 
				<select name="categorie" id="categorie" size="1">
				<c:forEach items="${categories}" var="category">
					<c:choose>
    				<c:when test="${game.getJoc_categorie().getJoc_categorie_id() == category.getJoc_categorie_id()}">
                		<option selected value="${category.getJoc_categorie_id()}">${category.getNume()}</option>
         			</c:when>
         			<c:otherwise>
         				<option value="${category.getJoc_categorie_id()}">${category.getNume()}</option>
         			</c:otherwise>
					</c:choose>
				</c:forEach>
				</select>
			</div>
			<div class="form-group">
				<label for="producator">Producator:</label>
				<select name="producator" id="producator" size="1">
				<c:forEach items="${producers}" var="producer">
					<c:choose>
    				<c:when test="${game.getJoc_producator().getJoc_producator_id() == producer.getJoc_producator_id()}">
                		<option selected value="${producer.getJoc_producator_id()}">${producer.getNume()}</option>
         			</c:when>
         			<c:otherwise>
         				<option value="${producer.getJoc_producator_id()}">${producer.getNume()}</option>
         			</c:otherwise>
					</c:choose>
				</c:forEach>
				</select>
			</div>
			<div class="form-group">
				<label for="tip">Tip:</label> 
				<select name="tip" id="tip" size="1">
				<c:forEach items="${types}" var="type">
					<c:choose>
    				<c:when test="${game.getJoc_tip().getJoc_tip_id() == type.getJoc_tip_id()}">
                		<option selected value="${type.getJoc_tip_id()}">${type.getNume()}</option>
         			</c:when>
         			<c:otherwise>
         				<option value="${type.getJoc_tip_id()}">${type.getNume()}</option>
         			</c:otherwise>
					</c:choose>
				</c:forEach>
				</select>
			</div>
			<div class="form-group">
				<label for="jucatori">Numar jucatori:</label> 
				<select name="jucatori" id="jucatori" size="1">
				<c:forEach items="${players}" var="player">
					<c:choose>
    				<c:when test="${game.getJocNrJucatori().getJoc_numar_jucatori_id() == player.getJoc_numar_jucatori_id()}">
                		<option selected value="${player.getJoc_numar_jucatori_id()}">${player.getValoare()}</option>
         			</c:when>
         			<c:otherwise>
         				<option value="${player.getJoc_numar_jucatori_id()}">${player.getValoare()}</option>
         			</c:otherwise>
					</c:choose>
				</c:forEach>
				</select>
			</div>
			<button type="submit" class="btn btn-default">Salvare</button>
		</form>
	</div>
</body>
</html>