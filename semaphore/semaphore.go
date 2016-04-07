package semaphore

import (
	"syscall"
	"unsafe"
)

const (
	IPC_CREAT int = 01000
	IPC_RMID  int = 0
	SETVAL    int = 16
	GETVAL    int = 12
)

type Semaphore struct {
	semid int
	nsems int
}

type semop struct {
	semNum  uint16
	semOp   int16
	semFlag int16
}

func errnoErr(errno syscall.Errno) error {
	switch errno {
	case syscall.Errno(0):
		return nil
	default:
		return error(errno)
	}
}

func SemGet(key int, nsems int, flags int) (*Semaphore, error) {
	r1, _, errno := syscall.Syscall(syscall.SYS_SEMGET,
		uintptr(key), uintptr(nsems), uintptr(flags))
	if errno == syscall.Errno(0) {
		return &Semaphore{semid: int(r1), nsems: nsems}, nil
	} else {
		return nil, errnoErr(errno)
	}
}

func (s *Semaphore) Destroy() error {
	_, _, errno := syscall.Syscall(syscall.SYS_SEMCTL, uintptr(s.semid),
		uintptr(0), uintptr(IPC_RMID))
	return errnoErr(errno)
}

func (s *Semaphore) GetVal(semNum int) (int, error) {
	val, _, errno := syscall.Syscall(syscall.SYS_SEMCTL, uintptr(s.semid),
		uintptr(semNum), uintptr(GETVAL))
	return int(val), errnoErr(errno)
}

func (s *Semaphore) Post(semNum int) error {
	post := semop{semNum: uint16(semNum), semOp: 1, semFlag: 0x1000}
	_, _, errno := syscall.Syscall(syscall.SYS_SEMOP, uintptr(s.semid),
		uintptr(unsafe.Pointer(&post)), uintptr(s.nsems))
	return errnoErr(errno)

}

func (s *Semaphore) Wait(semNum int) error {
	wait := semop{semNum: uint16(semNum), semOp: -1, semFlag: 0x1000}
	_, _, errno := syscall.Syscall(syscall.SYS_SEMOP, uintptr(s.semid),
		uintptr(unsafe.Pointer(&wait)), uintptr(s.nsems))
	return errnoErr(errno)
}
