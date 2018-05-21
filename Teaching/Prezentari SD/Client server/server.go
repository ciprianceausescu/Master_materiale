package main

import "net"
import "fmt"
import "bufio"

import (
	"strings"
	"strconv"
) // only needed below for sample processing

//func medie_ar(){

//}

func main() {

fmt.Println("Launching server...")


ln, _ := net.Listen("tcp", ":8080")
ln1, _ := net.Listen("tcp", ":8081")
ln2, _ := net.Listen("tcp", ":8082")

fmt.Println("1")

conn, _ := ln.Accept()
conn1, _ := ln1.Accept()
conn2, _ := ln2.Accept()

fmt.Println("2")

for {

	handleRequest(conn)
	handleRequest(conn1)
	handleRequest(conn2)


}
}

func handleRequest(conn net.Conn) {
	message, _ := bufio.NewReader(conn).ReadString('\n')
	message = strings.TrimRight(message, "\n")

	var medie  int
	var sum int = 0
	var nr int = 0

	var b= strings.Split(message, "#")
	for i := 0; i < len(b); i++ {
		a, _ := strconv.Atoi(b[i])
		sum = sum + a
		nr = nr + 1
	}
	medie = int(sum / nr)
	newmessage := string(strconv.Itoa(medie));
	conn.Write([]byte(newmessage + "\n"))
	conn.Close()
}