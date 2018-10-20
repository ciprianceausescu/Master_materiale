<%@ page language="java" contentType="text/html; charset=UTF-8"
 pageEncoding="UTF-8"%>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>  
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<title>Home Page</title>
</head>
<body>
<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css" />
    
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
      <h3>Accomodations</h3>
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
	            <a href="" class="btn btn-outline-success">Edit</a>
	            <a href="" class="btn btn-outline-success">Delete</a>
	          </td>
	        </tr>
       </c:forEach>
      </tbody>
    </table>
    </div>

 
	Countries:
	<table border="1" cellpadding="5" cellspacing="1" class="table table-bordered">
       <tr>
          <th>id</th>
          <th>Name</th>
          <th>Name</th>
       </tr>
       <c:forEach items="${regionList}" var="region" >
          <tr>
             <td>${region.id}</td>
             <td>${region.name}</td>
             <td>
                <a href="editProduct?code=${region.id}">Edit</a>
             </td>
             <td>
                <a href="deleteProduct?code=${region.id}">Delete</a>
             </td>
          </tr>
       </c:forEach>
    </table>
    
	
	
</body>
</html>