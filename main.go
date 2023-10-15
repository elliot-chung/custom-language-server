// simple http web server

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"
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

	var createError = func(msg string) {
		res.Output = msg
		json.NewEncoder(w).Encode(res)
		del()
	}

	// Create a directory for the user
	err := os.Mkdir(fmt.Sprintf("tests/tmp%d", id), 0777)
	if err != nil {
		createError(fmt.Sprintf("Server Error creating directory: %s", err))
		return
	}

	// Parse the request body
	var req CompilationRequest
	err = json.NewDecoder(r.Body).Decode(&req)
	if err != nil {
		createError(fmt.Sprintf("Server Error parsing request body: %s", err))
		return
	}

	// Create a file with the request body
	f, err := os.Create(fmt.Sprintf("tests/tmp%d/code.snek", id))
	if err != nil {
		createError(fmt.Sprintf("Server Error creating file: %s", err))
		return
	}
	defer f.Close()

	// Write the code to the file
	_, err = f.Write([]byte(req.Code))
	if err != nil {
		createError(fmt.Sprintf("Server Error writing to file: %s", err))
		return
	}

	// Compile the file
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	cmd := exec.CommandContext(ctx, "make", fmt.Sprintf("tests/tmp%d/code.run", id))
	stderr, err := cmd.StderrPipe()
	if err != nil {
		createError(fmt.Sprintf("Server Error creating stderr pipe: %s", err))
		return
	}

	cmd.Start()
	slurp, _ := io.ReadAll(stderr)

	err = cmd.Wait()
	if err != nil {
		// If ctx was canceled, then the command timed out
		if ctx.Err() == context.DeadlineExceeded {
			createError("Error: Timed out during compilation")
			return
		}
		createError(fmt.Sprintf("Error compiling file: %s", slurp))
		return
	}

	// Run the file
	inp := "false"
	if req.Input != "" {
		// Sanitize the input by removing all whitespace character (including newlines)
		var b strings.Builder
		for _, c := range req.Input {
			if !strings.ContainsRune(" \t\n\r", c) {
				b.WriteRune(c)
			}
		}
		inp = b.String()
	}
	ctx, cancel = context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	cmd = exec.CommandContext(ctx, fmt.Sprintf("./tests/tmp%d/code.run", id), inp, strconv.FormatInt(req.Memory, 10))

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		createError(fmt.Sprintf("Server Error creating stdout pipe: %s", err))
		return
	}
	stderr, err = cmd.StderrPipe()
	if err != nil {
		createError(fmt.Sprintf("Server Error creating stderr pipe: %s", err))
		return
	}

	cmd.Start()
	slurperr, _ := io.ReadAll(stderr)
	slurpout, _ := io.ReadAll(stdout)

	err = cmd.Wait()
	if err != nil {
		// If ctx was canceled, then the command timed out
		if ctx.Err() == context.DeadlineExceeded {
			createError("Error: Timed out during execution")
			return
		}
		createError(fmt.Sprintf("Error running file: %s", slurperr))
		return
	}

	// Return the output
	res.Success = true
	res.Output = string(slurpout)
	json.NewEncoder(w).Encode(res)

	// Delete the directory
	del()
}

func getID() int {
	id += 1
	id = id % 100000
	return id
}
