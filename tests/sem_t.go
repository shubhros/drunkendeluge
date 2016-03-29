package main

import (
	"fmt"
	"syscall"
	"time"
	"unsafe"
)

type semop struct {
	semNum  uint16
	semOp   int16
	semFlag int16
}

func main() {
	key := 12349
	num := 1
	flags := 01777
	wait := semop{semNum: 0, semOp: 1, semFlag: 0x1000}
	//signal := semop{semNum: 0, semOp: -1, semFlag: 0x1000}
	a, b, c := syscall.Syscall(syscall.SYS_SEMGET, uintptr(key), uintptr(num), uintptr(flags))
	fmt.Println(a, b, c)

	x, y, z := syscall.Syscall(syscall.SYS_SEMOP, a, uintptr(unsafe.Pointer(&wait)), uintptr(num))

	fmt.Println(x, y, z)
	time.Sleep(time.Second * 100)
}
