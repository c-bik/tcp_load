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
#include <sys/epoll.h>
#include <unistd.h>
#include <fcntl.h>

#define MAX_EVENTS 10
#define BUF_SIZE 1024

int main(int argc, char *argv[])
{
    struct epoll_event ev, events[MAX_EVENTS];
    int listen_sock, conn_sock, nfds, epollfd;
    struct sockaddr_in serv_addr;
    pid_t childPid;

    listen_sock = socket(AF_INET, SOCK_STREAM, 0);
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

    bind(listen_sock, (struct sockaddr*)&serv_addr, sizeof(serv_addr));

    listen(listen_sock, 10);

    epollfd = epoll_create(10);
    if (epollfd == -1) {
        perror("epoll_create");
        exit(EXIT_FAILURE);
    }

    ev.events = EPOLLIN;
    ev.data.fd = listen_sock;
    if (epoll_ctl(epollfd, EPOLL_CTL_ADD, listen_sock, &ev) == -1) {
        perror("epoll_ctl: listen_sock");
        exit(EXIT_FAILURE);
    }

    struct sockaddr_in local;
    socklen_t addrlen;
    char buf[BUF_SIZE];
    memset(buf, 0, BUF_SIZE);
    for (;;) {
        nfds = epoll_wait(epollfd, events, MAX_EVENTS, -1);
        if (nfds == -1) {
            perror("epoll_pwait");
            exit(EXIT_FAILURE);
        }

        for (int n = 0; n < nfds; ++n) {
            if (events[n].data.fd == listen_sock) {
                conn_sock = accept(listen_sock,
                                (struct sockaddr *) &local, &addrlen);
                if (conn_sock == -1) {
                    perror("accept");
                    exit(EXIT_FAILURE);
                }
                if (fcntl(conn_sock, F_SETFL, fcntl(conn_sock, F_GETFL, 0) | O_NONBLOCK)
                     < 0) {
                    perror("calling fcntl");
                    exit(EXIT_FAILURE);
                }
                ev.events = EPOLLIN | EPOLLET;
                ev.data.fd = conn_sock;
                if (epoll_ctl(epollfd, EPOLL_CTL_ADD, conn_sock,
                            &ev) == -1) {
                    perror("epoll_ctl: conn_sock");
                    exit(EXIT_FAILURE);
                }
            } else {
                while(true) {
                    ssize_t len = recv(events[n].data.fd, buf, BUF_SIZE, MSG_DONTWAIT);
                    if (len < 0) {
                        if (errno == EAGAIN || errno == EWOULDBLOCK)
                        break;
                    } else if (len > 0) {
                        printf("RX :");
                        for(int i = 0; i < len; ++i) {
                            printf("%02X ", buf[i]);
                        }
                        printf("\n");
                    } else {
                        printf("peer closed\n");
                        break;
                    }
                }
            }
        }
    }
    return 0;
}
