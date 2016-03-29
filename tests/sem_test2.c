#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>
#include<sys/sem.h>
#include<sys/ipc.h>
#include<sys/types.h>
#include<string.h>
#include<errno.h>
#include <fcntl.h>


int main(void)
{
	struct sembuf wait;
	struct sembuf signal;

	wait.sem_num = 0;
	wait.sem_op = -1;
	wait.sem_flg = SEM_UNDO;

	signal.sem_num = 0;
	signal.sem_op = 1;
	signal.sem_flg = SEM_UNDO;

	int semid = semget(12345, 1, IPC_CREAT|0600);
	printf("Allocating the semaphore: %s\n",strerror(errno));
	int semval = 1;

	printf ("Waiting for semaphore\n");
	semop(semid, &wait, 1);
	semval = semctl(semid, 0, GETVAL);
	printf("Setting semaphore value to %d: %s\n",semval,strerror(errno));
	printf ("semaphore acquired\n");

	getchar();



	semctl(semid, 0, IPC_RMID);
}

