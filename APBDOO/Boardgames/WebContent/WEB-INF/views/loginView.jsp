<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Logare</title>
</head>
<body>
	<link rel="stylesheet"
		href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css" />
	<div>
		<h3>Logare</h3>
	</div>
	<div class="ui-field-contain">
		<form action="Login" method="post">
			<span>Utilizator:</span> 
			<input type="text" name="username"> <br>
			<span>Parola:</span> 
			<input type="password" name="parola"> <br>
			<input type="submit" value="login">
		</form>
	</div>
</body>
</html>