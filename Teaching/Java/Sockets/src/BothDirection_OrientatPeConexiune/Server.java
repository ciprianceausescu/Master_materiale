
package BothDirection_OrientatPeConexiune;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

/**
 *
 * @author CeachiBogdan
 */
public class Server {
      public static void main(String[] args) {
        try {
            
            System.out.println("(Server) Server pornit");
            ServerSocket ss = new ServerSocket(9999);
            
            System.out.println(" (Server) Server asteapta request de la client");
            Socket socket = ss.accept(); // daca serverul accepta orice request
            
            System.out.println("(Server) Client conectat");
            
            BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));// specifici de unde iei inputul
            String str = br.readLine();
            
            System.out.println(" (Server) Client data : "  + str);
            
            // de aici cititi
            String nume = str.substring(0,6);
            OutputStreamWriter os = new OutputStreamWriter(socket.getOutputStream());
            PrintWriter out = new PrintWriter(os);
            out.println(nume);  
            out.flush(); // forteaza trimitearea datelor
            System.out.println(" (Server) Date trimise de Server catre client : " + nume);
            
            
            
            
        } catch (IOException ex) {
            System.err.println(ex.getMessage());
        }
        
        
    }
}
