package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"os/exec"
	"io/ioutil"
	"os"
)

// Command-line flags.
var (
	httpAddr   = flag.String("http", ":8080", "Listen address")
	command		 = flag.String("cmd", "./bin/leo3", "Path to executable")
	args			 = flag.String("args", "-t 60", "Additional Arguments passed to leo3")
)

func main() {
	flag.Parse()
	http.HandleFunc("/", call_leo)
	log.Fatal(http.ListenAndServe(*httpAddr, nil))
}

func call_leo(w http.ResponseWriter, req *http.Request) {
	// Saving the input into a file
	b, err := ioutil.ReadAll(req.Body)
	if err != nil {
		log.Print(err)
	}
	tempfile, err := ioutil.TempFile("", "problem")
	if err != nil {
		log.Print(err)
	}
	defer os.Remove(tempfile.Name())

	if _, err := tempfile.Write(b); err != nil {
		log.Print(err)
	}

	// Executing Leo3
	out, err := exec.Command(*command, tempfile.Name(), *args).Output()
	if err != nil {
		log.Print(err)
	}
	fmt.Fprintf(w, "%s", out)
}
