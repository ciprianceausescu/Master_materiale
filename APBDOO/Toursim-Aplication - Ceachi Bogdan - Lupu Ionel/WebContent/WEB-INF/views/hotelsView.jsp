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
     <jsp:include page="_menu.jsp"></jsp:include>
	
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
      <h3>Hotels 
		<a href="Hotels?action=new" class="btn btn-outline-success">Insert</a>
	</h3>
      <div class="input-group">
        <input class="form-control" placeholder="Search accomodations">
        <div class="input-group-addon" ><i class="fa fa-search"></i></div>
      </div>
      <br>
      <table class="table table-bordered">
      <thead class="thead-inverse">
        <tr>
          <th scope="col">#</th>
          <th scope="col">Name</th>
          <th scope="col">Stars</th>
          <th scope="col">City</th>
          <th scope="col">Address</th>
          <th scope="col"></th>
        </tr>
      </thead>
      <tbody>
      <c:forEach items="${accomodations}" var="accomodation" >
	        <tr>
	          <th scope="row">#</th>
	          <td>${accomodation.getName()}</td>
	          <td>${accomodation.getStars()}</td>
	          <td>${accomodation.getAddress().getCity().getName()}</td>
	          <td>${accomodation.getAddress().getName()}</td>
	          <td>
	          	<a href="Hotels?action=edit&id=${accomodation.getId()}" class="btn btn-outline-success">Edit</a>
	          	<a href="Hotels?action=delete&id=${accomodation.getId()}" class="btn btn-outline-danger">Delete</a>
	          </td>
	        </tr>
       </c:forEach>
      </tbody>
    </table>
    </div>

</body>
</html>