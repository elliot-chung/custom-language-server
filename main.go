// simple http web server

package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"os/exec"
	"strconv"
)

var id = 0

func main() {
	http.HandleFunc("/", handler)
	http.HandleFunc("/compile", handler2)
	http.ListenAndServe(":8080", nil)
}

func enableCors(w *http.ResponseWriter) {
	(*w).Header().Set("Access-Control-Allow-Origin", "*")
	(*w).Header().Set("Access-Control-Allow-Credentials", "true")
	(*w).Header().Set("Access-Control-Allow-Methods", "GET,HEAD,OPTIONS,POST,PUT")
	(*w).Header().Set("Access-Control-Allow-Headers", "Access-Control-Allow-Headers, Origin,Accept, X-Requested-With, Content-Type, Access-Control-Request-Method")
}

func handler(w http.ResponseWriter, r *http.Request) {
	enableCors(&w)

	w.Write([]byte("Hello World!"))
}

func handler2(w http.ResponseWriter, r *http.Request) {
	enableCors(&w)

	w.Header().Set("Content-Type", "application/json")

	type CompilationRequest struct {
		Memory int64  `json:"memory"`
		Input  string `json:"input"`
		Code   string `json:"code"`
	}

	type CompilationResponse struct {
		Success bool   `json:"success"`
		Output  string `json:"output"`
	}

	var res CompilationResponse
	res.Success = false

	id := getID()

	var del = func() {
		err := os.RemoveAll(fmt.Sprintf("tests/tmp%d", id))
		if err != nil {
			fmt.Printf("Error deleting directory: %s", err)
		}
	}

	// Create a directory for the user
	err := os.Mkdir(fmt.Sprintf("tests/tmp%d", id), 0777)
	if err != nil {
		res.Output = fmt.Sprintf("Error creating directory: %s", err)
		json.NewEncoder(w).Encode(res)
		del()
		return
	}

	// Parse the request body
	var req CompilationRequest
	err = json.NewDecoder(r.Body).Decode(&req)
	if err != nil {
		res.Output = fmt.Sprintf("Error parsing request body: %s", err)
		json.NewEncoder(w).Encode(res)
		del()
		return
	}

	// Create a file with the request body
	f, err := os.Create(fmt.Sprintf("tests/tmp%d/code.snek", id))
	if err != nil {
		res.Output = fmt.Sprintf("Error creating file: %s", err)
		json.NewEncoder(w).Encode(res)
		del()
		return
	}
	defer f.Close()

	// Write the code to the file
	_, err = f.Write([]byte(req.Code))
	if err != nil {
		res.Output = fmt.Sprintf("Error writing to file: %s", err)
		json.NewEncoder(w).Encode(res)
		del()
		return
	}

	// Compile the file
	_, err = exec.Command("make", fmt.Sprintf("tests/tmp%d/code.run", id)).Output()
	if err != nil {
		res.Output = fmt.Sprintf("Error compiling file: %s", err)
		json.NewEncoder(w).Encode(res)
		del()
		return
	}

	// Run the file
	inp := "false"
	if req.Input != "" {
		inp = req.Input
	}
	out, err := exec.Command(fmt.Sprintf("./tests/tmp%d/code.run", id), inp, strconv.FormatInt(req.Memory, 10)).Output()
	if err != nil {
		res.Output = fmt.Sprintf("Error running file: %s", err)
		json.NewEncoder(w).Encode(res)
		del()
		return
	}

	// Return the output
	res.Success = true
	res.Output = string(out)
	json.NewEncoder(w).Encode(res)

	// Delete the directory
	del()
}

func getID() int {
	id += 1
	id = id % 100000
	return id
}
