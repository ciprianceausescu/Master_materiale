/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Neorientat_Pe_Conexiune;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author CeachiBogdan
 */
public class Client {
    static final int PORT = 2003;
    public static void main(String[] args) {
        BufferedReader citireTastatura;
         citireTastatura = new BufferedReader(new InputStreamReader(System.in));
         System.out.flush();
        DatagramSocket socket = null;
        
        try {
            socket = new DatagramSocket();
            
        } catch (SocketException ex) {
           System.err.println("Nu putem crea socketul" + ex.getMessage());
           System.exit(1);
        }
        
        long timp = 0;
        
        byte[] buffer = new byte[1];
        try {
            String linie = citireTastatura.readLine();
            socket.send(new DatagramPacket(buffer, 1,
                    InetAddress.getByName(linie), PORT));
            DatagramPacket raspuns = new DatagramPacket(new byte[8], 8);
            socket.receive(raspuns);
            ByteArrayInputStream baos;
            baos = new ByteArrayInputStream(raspuns.getData());
            DataInputStream dis = new DataInputStream(baos);
            timp = dis.readLong();
            
        } catch (IOException ex) {
            System.err.println("Nu putem trimite sau primi datagrame! "
                                + ex.getMessage());
        }
        System.out.println("Timpul primit de la server este " + new Date(timp));
        socket.close();
    }
}
