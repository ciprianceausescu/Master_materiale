<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Acasa</title>
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
		<h3>Acasa</h3>
		<br>
		Bine ati venit la Libraria cu jocuri !
	</div>
</body>
</html>