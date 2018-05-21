package main.java.com.memorynotfound.ws;

import javax.jws.WebMethod;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;
import javax.jws.soap.SOAPBinding.Style;
import java.util.List;

@WebService
@SOAPBinding(style = Style.RPC)
public interface OrderService {

    @WebMethod
    String[] getOrders();

    @WebMethod
    boolean addOrder(String order);

}
