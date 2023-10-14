// simple http web server

package main

import (
	"fmt"
	"net/http"
	"os"
	"os/exec"
)

var id = 0

func main() {
	http.HandleFunc("/", handler)
	http.HandleFunc("/compile", handler2)
	http.ListenAndServe(":8080", nil)
}

func handler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello, %s!", r.URL.Path[1:])
}

func handler2(w http.ResponseWriter, r *http.Request) {
	id := getID()

	// Create a directory for the user
	err := os.Mkdir(fmt.Sprintf("tests/tmp%d", id), 0777)
	if err != nil {
		fmt.Fprintf(w, "Error: %s", err)
	}

	// Parse the request body
	body := "69420"

	// Create a file with the request body
	f, err := os.Create(fmt.Sprintf("tests/tmp%d/code.snek", id))
	if err != nil {
		fmt.Fprintf(w, "Error: %s", err)
	}
	defer f.Close()
	_, err = f.Write([]byte(body))
	if err != nil {
		fmt.Fprintf(w, "Error: %s", err)
	}

	// Compile the file
	_, err = exec.Command("make", fmt.Sprintf("tests/tmp%d/code.run", id)).Output()
	if err != nil {
		fmt.Fprintf(w, "Error: %s", err)
	}

	// Run the file
	out, err := exec.Command(fmt.Sprintf("./tests/tmp%d/code.run", id)).Output()
	if err != nil {
		fmt.Fprintf(w, "Error: %s", err)
	}

	// Delete the directory
	err = os.RemoveAll(fmt.Sprintf("tests/tmp%d", id))
	if err != nil {
		fmt.Fprintf(w, "Error: %s", err)
	}

	// Return the output
	fmt.Fprintf(w, "Output: %s", out)
}

func getID() int {
	id += 1
	id = id % 100000
	return id
}
