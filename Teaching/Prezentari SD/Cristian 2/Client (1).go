package main

import "net"
import "fmt"
import (
	"bufio"
	"time"
	"strings"
)


func main() {

	// connect to this socket
	conn, _ := net.Dial("tcp", "127.0.0.1:8081")
	for {
		// read in input from stdin
        start := time.Now()
		fmt.Println("Text to send: " + start.String())
		//reader := bufio.NewReader(start.String())
		// send to socket
		fmt.Fprintf(conn, start.String() + "\n")
		// listen for reply
		message, _ := bufio.NewReader(conn).ReadString('\n')
		newMsg := strings.Split(message, "\n")
		serverTime := newMsg[0]
		server, _ := time.ParseDuration(serverTime)
		fmt.Println("Message from server: " + serverTime)
		fmt.Println("Server Time: " + server.String())
		finish := time.Since(start) / 2
		fmt.Println("Finish time: " + finish.String())
		syncTime := finish + server
		fmt.Println("Client  time now: " + syncTime.String())
		break
	}
}
