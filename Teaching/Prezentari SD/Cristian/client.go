package main

import (
	"bufio"
	"fmt"
	"net"
	"os"
	"time"
)

func main() {

	message2("Launching client")
	conn, err := net.Dial("tcp", "127.0.0.1:8081")
	exitOnError2(err)

	before := time.Now()
	message2("")
	message2("Before:  ", before)
	fmt.Fprintf(conn, before.Format(time.RFC3339Nano)+"\n")

	str, err := bufio.NewReader(conn).ReadString('\n')
	exitOnError2(err)
	if len(str) < 1 {
		message2("ERR: Invalid string!")
		os.Exit(1)
	}

	received, err := time.Parse(time.RFC3339Nano, str[:len(str)-1])
	exitOnError2(err)
	message2("Received:", received)

	after := time.Now()
	message2("After:   ", after)

	correction := after.Sub(before) / 2

	message2("")
	message2("Correction: +", correction)
	message2("Time is", received.Add(correction))
}

func message2(a ...interface{}) (n int, err error) {
	return fmt.Print("[C] ", fmt.Sprintln(a...))
}

func exitOnError2(err error) {
	if err != nil {
		message2("ERR:", err)
		os.Exit(1)
	}
}