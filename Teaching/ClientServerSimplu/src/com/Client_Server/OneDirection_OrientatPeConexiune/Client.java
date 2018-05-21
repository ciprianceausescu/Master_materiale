
package com.Client_Server.OneDirection_OrientatPeConexiune;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

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
            os.write(str);         
            
           // os.flush();
            
            os.close();
            
        } catch (IOException ex) {
             System.err.println(ex.getMessage());
        }
        
    }
}
