<%@ page import="java.util.ArrayList" %>
<%@ page import="test.Person" %>
<%@ page language="java" contentType="text/html; charset=UTF-8"
         pageEncoding="UTF-8"%>

<head>
    <title>Title</title>
</head>
<body>
<table class="table table-bordered">
    <thead class="thead-inverse">
        <tr>
            <th scope="col">Nume</th>
            <th scope="col">Varsta</th>
        </tr>
    </thead>
    <tbody>
    <%
        ArrayList<Person> persons = (ArrayList<Person>)request.getAttribute("persons");
        for(int i = 0; i < persons.size(); i+=1) { %>
        <tr>
            <td><%=persons.get(i).getName()%></td>
            <td><%=persons.get(i).getAge()%></td>
        </tr>
    <% } %>
    </tbody>
</table>
</body>
</html>
