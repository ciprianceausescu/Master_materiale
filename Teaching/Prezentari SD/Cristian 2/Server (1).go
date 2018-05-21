package main

import "net"
import "fmt"
import "bufio"
import (
	"time"
) // only needed below for sample processing

func main() {

	fmt.Println("Launching server...")

	// listen on all interfaces
	ln, _ := net.Listen("tcp", ":8081")

	// accept connection on port
	conn, _ := ln.Accept()

	// run loop forever (or until ctrl-c)
	for {
		// will listen for message to process ending in newline (\n)
		message, _ := bufio.NewReader(conn).ReadString('\n')
		// output message received
		fmt.Println("Message Received:", string(message))
		serverTime := time.Now()
		// send new string back to client
		conn.Write([]byte(serverTime.String() + "\n"))
		fmt.Println("serverTime: " + serverTime.String())
		break
	}
}