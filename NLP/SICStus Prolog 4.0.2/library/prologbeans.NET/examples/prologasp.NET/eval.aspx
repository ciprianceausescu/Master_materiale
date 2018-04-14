<%@ Page Language="C#" Debug="true" %>
<%@ Import Namespace="se.sics.prologbeans" %>

<html>
   <head><title>Evaluator</title></head>
   <body>
      <center>
      <form>
         <h3> Expression: <input name="Expr" type=text value="<%=Request.QueryString["Expr"]%>">
            <%
               PrologSession pSession = new PrologSession();
               String evQuery = Request.QueryString["Expr"];
               String output = "";
	       if (evQuery != null) {
		 Bindings bindings = new Bindings().bind("E",evQuery + '.');
                 session.connect();     /* This will connect if necessary. */
		 QueryAnswer answer = pSession.executeQuery("evaluate(E,R)", bindings);
		 PBTerm result = answer.getValue("R");

		 if (result != null) {
		   output = "<h4>Result = " + result + "</h4>";
		 } else {
		   output = "<h4>Error: " + answer.Error + "</h4>";
		 }
	       }
            %>
       </form>

       </center>

       <%= output  %><br>
       </font>
       <p><hr>Powered by SICStus Prolog

   </body>
</html>
