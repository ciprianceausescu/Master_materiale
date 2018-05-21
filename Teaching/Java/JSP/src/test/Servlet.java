package test;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Created by Ciprian Mihai on 5/8/2018.
 */
@WebServlet("/Servlet")
public class Servlet extends javax.servlet.http.HttpServlet {
    protected void doPost(javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws javax.servlet.ServletException, IOException {
        doGet(request, response);
    }

    protected void doGet(javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws javax.servlet.ServletException, IOException {
        String action = "list";

        switch (action) {
            case "list":
                list(request, response);
                break;
        }
    }

    private void list(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        ArrayList<Person> personsList = new ArrayList<>();


        personsList.add(new Person("Ion", 20));
        personsList.add(new Person("Maria", 17));
        personsList.add(new Person("Izabela", 21));
        personsList.add(new Person("Marcel", 23));

        request.setAttribute("persons", personsList);

        RequestDispatcher dispatcher = request.getServletContext().getRequestDispatcher("/test.jsp");
        dispatcher.forward(request, response);
    }
}
