package com.Client_Server.OneDirection_OrientatPeConexiune;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author CeachiBogdan
 */
public class Server {
    
    public static void main(String[] args) {
        try {
            
            System.out.println("Server started");
            ServerSocket ss = new ServerSocket(9999);
            
            System.out.println("Server is waiting for client request");
            Socket socket = ss.accept(); // daca serverul accepta orice request
            
            System.out.println("Client conectat");
            
            BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));// specifici de unde iei inputul
            String str = br.readLine();
            
            System.out.println("Client data : "  + str);
            
            
            
        } catch (IOException ex) {
            System.err.println(ex.getMessage());
        }
        
        
    }
    
}
