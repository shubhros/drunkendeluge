#define _GNU_SOURCE
#include <unistd.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

int main(void)
{
	unlink("/tmp/test_fifo");
	int ret = mkfifo("/tmp/test_fifo", 0644);
	int nbytes = 0;
	int totalbytes = 0;
	uint8_t c = 0x0a;
	int size = 1024;

	if (ret < 0) {
		perror("Error creating fifo");
		return -1;
	}

	printf ("Ok! fifo created opening it\n");

	int fd = open("/tmp/test_fifo", O_WRONLY);
	if (fd < 0) {
		perror("Error opening fifo\n");
		return -1;
	}

	ret = fcntl(fd, F_SETPIPE_SZ, size);
	if (ret != 0) {
		perror("Error settting pipe size");
	}

	ret = fcntl(fd, F_GETPIPE_SZ);
	printf ("Pipe size set: %d\n", ret);




	printf ("Ok! fifo fd: %d\n", fd);
	while(1) {
		nbytes = write(fd, &c, 1);
		if (nbytes == 0) {
			printf ("Error writing to the pipe\n");
			return -1;
		}
		totalbytes += nbytes;
		printf ("Wrote %d bytes\n", totalbytes);
	}

	return 0;
}
