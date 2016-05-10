#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>

void dump(void *buf, size_t buf_len)
{
    for(size_t i = 0; i < buf_len; ++i)
        printf("%02X ", ((unsigned char *)buf)[i]);
    printf("\n");
}

int main(int argc, char *argv[])
{
    int listenfd = 0, connfd = 0;
    struct sockaddr_in serv_addr;
    pid_t childPid; 

    listenfd = socket(AF_INET, SOCK_STREAM, 0);
    memset(&serv_addr, '0', sizeof(serv_addr));

    serv_addr.sin_family = AF_INET;
    if(argc > 2) {
        if (inet_pton(AF_INET, argv[1], &(serv_addr.sin_addr.s_addr)) <= 0) {
            printf("\n Usage: %s <listen ip> <listen port> \n", argv[0]);
            exit(0);
        }
        serv_addr.sin_port = htons(atoi(argv[2]));
        printf("listening %s %s\n", argv[1], argv[2]);
    } else {
        printf("\n Usage: %s <listen ip> <listen port> \n", argv[0]);
        exit(0);
    }

    bind(listenfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr));

    listen(listenfd, 10);

    struct timespec rtt, time;
    memset(&rtt, 0, sizeof(rtt));
    while(true) {
        connfd = accept(listenfd, (struct sockaddr*)NULL, NULL);

        childPid = fork();
        if (childPid < 0) {
            printf("[%d] failed to fork\n", getpid());
            break;
        } else if (childPid == 0) {
            while(true) {
                if(recv(connfd, &rtt, sizeof(rtt), MSG_WAITALL) < sizeof(rtt)) {
                    printf("[%d,%d] receive failed!\n", getppid(), getpid());
                    break;
                }
                printf("[%d,%d] Dump RTT ", getppid(), getpid());
                dump(&rtt, sizeof(rtt));

                memset(&time, 0, sizeof(time));
                if(clock_gettime(CLOCK_REALTIME_COARSE, &time) < 0) {
                    printf("[%d,%d] error getting time\n", getppid(), getpid());
                    break;
                }
                printf("[%d,%d] RTT %.5f s\n",
                    ((double)time.tv_sec + 1.0e-9*time.tv_nsec) - 
                    ((double)rtt.tv_sec + 1.0e-9*rtt.tv_nsec), getppid(), getpid());

                printf("Dump TIME ");
                dump(&time, sizeof(time));
                if(send(connfd, &rtt, sizeof(rtt), 0) < 0) {
                    printf("[%d,%d] socket send failed!\n", getppid(), getpid());
                    break;
                }
            }

            close(connfd);
        } else {
            sleep(1);
        }
    }
}
