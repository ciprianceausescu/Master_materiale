
package com.Client_Server.BothDirection_OrientatPeConexiune;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;

/**
 *
 * @author CeachiBogdan
 */
public class Client {
    public static void main(String[] args) {
         try {
            String ip = "localhost";
            int port = 9999;
            
            Socket socket = new Socket(ip, port);
            
            String str = "Ceachi Bogdan";
            
            OutputStreamWriter os = new OutputStreamWriter(socket.getOutputStream());
            PrintWriter out = new PrintWriter(os);
            out.println(str);         
            
            os.flush();
            
            
            // acceptam date
            BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));// specifici de unde iei inputul
            String nume = br.readLine();
            
            System.out.println("(Client) Date de la Server " + nume);
            
            os.close();
            
        } catch (IOException ex) {
             System.err.println(ex.getMessage());
        }
    }
}
