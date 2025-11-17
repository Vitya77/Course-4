package main

import (
	"bufio"
	"fmt"
	"net"
	"strings"
	"sync"
)

type Client struct {
	conn net.Conn
	name string
}

var (
	clients   = make(map[net.Conn]Client)
	broadcast = make(chan string)
	mutex     = &sync.Mutex{}
)

func main() {
	listener, err := net.Listen("tcp", ":8080")
	if err != nil {
		fmt.Println("Помилка запуску сервера:", err)
		return
	}
	defer listener.Close()

	fmt.Println("Сервер запущено на порту 8080")

	go handleBroadcast()

	for {
		conn, err := listener.Accept()
		if err != nil {
			fmt.Println("Помилка підключення:", err)
			continue
		}
		go handleConnection(conn)
	}
}

func handleConnection(conn net.Conn) {
	defer conn.Close()
	conn.Write([]byte("Введіть ваше ім'я: \n"))

	reader := bufio.NewReader(conn)
	nameInput, err := reader.ReadString('\n')
	if err != nil {
		return
	}
	name := strings.TrimSpace(nameInput)
	if name == "" {
		name = "Анонім"
	}

	mutex.Lock()
	clients[conn] = Client{conn: conn, name: name}
	mutex.Unlock()

	broadcast <- fmt.Sprintf("%s приєднався до чату!\n", name)
	conn.Write([]byte("Ви приєдналися до чату. Напишіть повідомлення:\n"))

	for {
		msg, err := reader.ReadString('\n')
		if err != nil {
			mutex.Lock()
			delete(clients, conn)
			mutex.Unlock()
			broadcast <- fmt.Sprintf("%s вийшов з чату. :(\n", name)
			return
		}
		msg = strings.TrimSpace(msg)
		if msg != "" {
			broadcast <- fmt.Sprintf("[%s]: %s\n", name, msg)
		}
	}
}

func handleBroadcast() {
	for {
		msg := <-broadcast
		mutex.Lock()
		for _, client := range clients {
			client.conn.Write([]byte(msg))
		}
		mutex.Unlock()
	}
}
