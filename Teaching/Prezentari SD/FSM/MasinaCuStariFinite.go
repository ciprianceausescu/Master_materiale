package main

import "fmt"

var currState = "Locked"

func MSF(state string){
	if state == "coin" {
		switch {
		case currState == "Locked":
			currState = "Unlocked"
		case currState == "Unlocked":
			currState = "Unlocked"
		}
	}
	if state == "push" {
		switch {
		case currState == "Locked":
			currState = "Locked"
		case currState == "Unlocked":
			currState = "Locked"
		}
	}
}
func main(){
	var state string
	fmt.Println(currState)
	for
	{
		fmt.Scanln(&state)
		MSF(state)
		fmt.Println(currState)
	}
}
