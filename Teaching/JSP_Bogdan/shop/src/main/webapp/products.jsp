<h3>Available offerings:</h3>

<c:forEach var="i" begin="0" end="${products.size() - 1}">

	<c:set var="product" scope="page" value="${products.get(i)}"/>  
	  
   	<p>
   		<b>${product.label}</b><br/>
   		<span>${product.description}</span> 
   	</p>
   	
</c:forEach>  