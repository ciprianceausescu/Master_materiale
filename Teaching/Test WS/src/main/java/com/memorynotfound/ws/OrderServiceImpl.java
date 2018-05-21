package main.java.com.memorynotfound.ws;

import javax.jws.WebService;

@WebService(endpointInterface = "main.java.com.memorynotfound.ws.OrderService")
public class OrderServiceImpl implements OrderService {

    public String[] getOrders() {
        return new String[]{"SSD", "Graphic Card", "GPU"};
    }

    public boolean addOrder(String order) {
        System.out.println("Saving new order: " + order);
        return true;
    }
}
