// Local static-file launcher. All application calculations remain in browser webR.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"log"
	"mime"
	"net"
	"net/http"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"runtime"
	"time"
)

func main() {
	noBrowser := flag.Bool("no-browser", false, "serve without opening a browser")
	flag.Parse()
	executable, err := os.Executable()
	if err != nil {
		log.Fatal(err)
	}
	executable, err = filepath.EvalSymlinks(executable)
	if err != nil {
		log.Fatal(err)
	}
	directory := filepath.Clean(filepath.Join(filepath.Dir(executable), "..", ".."))
	if _, err := os.Stat(filepath.Join(directory, "index.html")); err != nil {
		log.Fatal("Keep the launcher inside the extracted application folder: ", err)
	}
	root, err := os.OpenRoot(directory)
	if err != nil {
		log.Fatal(err)
	}
	defer root.Close()
	for extension, contentType := range map[string]string{
		".js": "text/javascript", ".mjs": "text/javascript",
		".wasm": "application/wasm", ".css": "text/css",
	} {
		if err := mime.AddExtensionType(extension, contentType); err != nil {
			log.Fatal(err)
		}
	}
	listener, err := net.Listen("tcp4", "127.0.0.1:0")
	if err != nil {
		log.Fatal("Cannot start local server: ", err)
	}
	files := http.FileServerFS(root.FS())
	server := &http.Server{
		ReadHeaderTimeout: 10 * time.Second,
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if r.Host != listener.Addr().String() {
				http.Error(w, "Invalid local address", http.StatusForbidden)
				return
			}
			if r.Method != http.MethodGet && r.Method != http.MethodHead {
				w.Header().Set("Allow", "GET, HEAD")
				http.Error(w, "Read-only static server", http.StatusMethodNotAllowed)
				return
			}
			files.ServeHTTP(w, r)
		}),
	}
	url := "http://" + listener.Addr().String() + "/index.html"
	fmt.Println("Cost & GHG Manager")
	fmt.Println(url)
	fmt.Println("Keep this window open. Save your work, then press Ctrl+C to stop.")
	stopped := make(chan os.Signal, 1)
	signal.Notify(stopped, os.Interrupt)
	defer signal.Stop(stopped)
	go func() {
		<-stopped
		ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
		defer cancel()
		_ = server.Shutdown(ctx)
	}()
	if !*noBrowser {
		go func() {
			var command *exec.Cmd
			switch runtime.GOOS {
			case "windows":
				command = exec.Command(filepath.Join(os.Getenv("SystemRoot"), "System32", "rundll32.exe"), "url.dll,FileProtocolHandler", url)
			case "darwin":
				command = exec.Command("/usr/bin/open", url)
			default:
				fmt.Println("Open the address above in your browser.")
				return
			}
			if err := command.Run(); err != nil {
				fmt.Println("Open the address above in your browser:", err)
			}
		}()
	}
	if err := server.Serve(listener); err != nil && !errors.Is(err, http.ErrServerClosed) {
		log.Fatal(err)
	}
}
