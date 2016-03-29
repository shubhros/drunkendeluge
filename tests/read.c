#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
	char b;
	int nbytes = 0;
	int count = 65535;
	int total_bytes = 0;

	int fd = open("/tmp/test_fifo", O_RDONLY);

	if (fd < 0) {
		perror("Error opening fifo\n");
		return -1;
	}

	printf ("Ok! fifo fd: %d\n", fd);
	for (int i =0; i < count; i++) {
		nbytes = read(fd, &b, 1);
		total_bytes += nbytes;
		printf ("bytes read: %d\n", total_bytes);
	}
	/*
	while(1) {
	}*/
	sleep(1000);

	return 0;
}
