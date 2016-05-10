#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <arpa/inet.h>
#include <time.h>

void dump(void *buf, size_t buf_len)
{
    for(size_t i = 0; i < buf_len; ++i)
        printf("%02X ", ((unsigned char *)buf)[i]);
    printf("\n");
}

void send_clock(int sockfd)
{
    struct timespec time;
    memset(&time, 0, sizeof(time));
    if(clock_gettime(CLOCK_REALTIME_COARSE, &time) < 0) {
        printf("[%d] error getting time\n", __LINE__);
        exit(0);
    }
    printf("Dump TIME ");
    dump(&time, sizeof(time));
    if(send(sockfd, &time, sizeof(time), 0) < 0) {
        printf("[%d] socket send failed!\n", __LINE__);
        exit(0);
    }
}

int main(int argc, char *argv[])
{
    int sockfd = 0, n = 0;
    struct sockaddr_in serv_addr;
    struct timespec rtt, time;

    if(argc != 3)
    {
        printf("\n Usage: %s <ip of server> \n",argv[0]);
        return 1;
    }

    if((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
        printf("\n Error : Could not create socket \n");
        return 1;
    }

    memset(&serv_addr, '0', sizeof(serv_addr));

    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(atoi(argv[2]));

    if(inet_pton(AF_INET, argv[1], &serv_addr.sin_addr)<=0)
    {
        printf("\n inet_pton error occured\n");
        return 1;
    }

    if( connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
    {
        printf("\n Error : Connect Failed \n");
        return 1;
    }

    send_clock(sockfd);
    while (true) {
        if(recv(sockfd, &rtt, sizeof(rtt), MSG_WAITALL) < sizeof(rtt)) {
            printf("receive failed!\n");
            exit(0);
        }
        printf("Dump RTT ");
        dump(&rtt, sizeof(rtt));

        memset(&time, 0, sizeof(time));
        if(clock_gettime(CLOCK_REALTIME_COARSE, &time) < 0) {
            printf("[%d] error getting time\n", __LINE__);
            exit(0);
        }
        printf("RTT %.5f s\n",
           ((double)time.tv_sec + 1.0e-9*time.tv_nsec) - 
           ((double)rtt.tv_sec + 1.0e-9*rtt.tv_nsec));

        printf("Dump TIME ");
        dump(&time, sizeof(time));

        sleep(5);
        send_clock(sockfd);
    }

    return 0;
}
