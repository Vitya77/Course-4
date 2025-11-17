package main

import (
	"bufio"
	"fmt"
	"net"
	"os"
)

func main() {
	conn, err := net.Dial("tcp", "localhost:8080")
	if err != nil {
		fmt.Println("Помилка підключення до сервера:", err)
		return
	}
	defer conn.Close()

	reader := bufio.NewReader(conn)

	msg, _ := reader.ReadString('\n')
	fmt.Print(msg)

	scanner := bufio.NewScanner(os.Stdin)
	if scanner.Scan() {
		fmt.Fprintf(conn, "%s\n", scanner.Text())
	}

	go readFromServer(reader)
	writeToServer(conn)
}

func readFromServer(reader *bufio.Reader) {
	for {
		message, err := reader.ReadString('\n')
		if err != nil {
			fmt.Println("З'єднання з сервером розірвано. :(")
			os.Exit(0)
		}
		fmt.Print(message)
	}
}

func writeToServer(conn net.Conn) {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		text := scanner.Text()
		if text == "/exit" {
			fmt.Println("Вихід з чату.")
			conn.Close()
			os.Exit(0)
		}
		fmt.Fprintf(conn, "%s\n", text)
	}
}
