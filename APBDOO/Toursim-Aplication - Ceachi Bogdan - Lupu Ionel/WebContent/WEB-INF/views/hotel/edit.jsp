<%@ page language="java" contentType="text/html; charset=UTF-8"
 pageEncoding="UTF-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>  
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Hotels Page</title>

	<link rel="stylesheet" href="<c:url value="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css" />" />
    
</head>
<body>
     <jsp:include page="../_menu.jsp"></jsp:include>
	
	<%	
			// spunei browserului sa nu memoreze nimic in cache, sa rezolvi problema cu back
			response.setHeader("Cache-Control", "no-cache, no-store, must-revalidate");
			response.setHeader("Pragma", "no-cache"); // same thing, but for HTTP 1.0
			 response.setHeader("Expires", "0");//Proxies
			if(session.getAttribute("username") == null) {
				response.sendRedirect("loginView.jsp");
			}
			 
	%>
	
	<div class="container">
      <h3>Hotel : ${ accomodation.getName() }</h3>
      <form action="?action=update&id=${ accomodation.getId() }" method="POST">
		  <div class="form-group">
		    <label for="email">Name:</label>
		    <input type="text" class="form-control" id="email" value="${accomodation.getName()}" name="name">
		  </div>
		  
		  
		  <div class="form-group">
		    <label for="type">Type:</label>
			<select class="form-control" id="type" name="type_id">
				<c:forEach items="${types}" var="type">
					<option value="${type.getId()}" ${accomodation.getAccomodation_type_id() == type.getId() ? 'selected' : ''}>
						${type.getName()}
					</option>
				</c:forEach>
			</select>
		  </div>
		  
		  <div class="form-group">
		    <label for="stars">Stars:</label>
		    <select class="form-control" id="stars" name="stars">
		    	<c:set var="stars">1,2,3,4,5,6,7</c:set>
				<c:forEach items="${stars}" var="star">
					<option value="${star}" ${accomodation.getStars() == star ? 'selected' : ''}>
						${star}
					</option>
				</c:forEach>
			  </select>
		  </div>
		  
		  
		  <div class="form-group">
		    <label for="description_short">Short description:</label>
		    <textarea class="form-control" rows="5" id="description_short" name="description_short">${accomodation.getDescription_short()}</textarea>
		  </div>
		  
		  <div class="form-group">
		    <label for="description_full">Full description:</label>
		    <textarea class="form-control" rows="5" id="description_full" name="description_full">${accomodation.getDescription_full()}</textarea>
		  </div>
		  
		  <div class="form-group">
		    <label for="address">Address:</label>
			<select class="form-control" id="address" name="address_id">
				<c:forEach items="${addresses}" var="address">
					<option value="${address.getId()}" ${accomodation.getAddress_id() == address.getId() ? 'selected' : ''}>
						${address.getName()}
					</option>
				</c:forEach>
			</select>
		  </div>
		  
		  <div class="form-group">
		    <label for="contact">Contact:</label>
			<select class="form-control" id="contact" name="contact_id">
				<c:forEach items="${contacts}" var="contact">
					<option value="${contact.getId()}" ${accomodation.getContact_id() == contact.getId() ? 'selected' : ''}>
						${contact.getEmail()}
					</option>
				</c:forEach>
			</select>
		  </div>
		  
		  <button type="submit" class="btn btn-default">Save</button>
		</form>
    </div>

</body>
</html>