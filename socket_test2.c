#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

/**
 * Test 1: Attempts to bind to IPv6 then IPv4 using system defaults.
 * This will reveal if the system uses dual-stack sockets by default.
 */
int run_default_test(int port)
{
    int ipv6_sock = -1, ipv4_sock = -1;
    struct sockaddr_in6 addr6;
    struct sockaddr_in addr4;
    int result = 0;

    printf("=== Test 1: System Default Socket Behavior ===\n");

    // Create IPv6 socket
    ipv6_sock = socket(AF_INET6, SOCK_STREAM, 0);
    if (ipv6_sock < 0)
    {
        printf("Error: Failed to create IPv6 socket: %s\n", strerror(errno));
        return -1;
    }

    // Set SO_REUSEADDR to avoid "Address already in use" errors
    int reuse = 1;
    if (setsockopt(ipv6_sock, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse)) < 0)
    {
        printf("Warning: Failed to set SO_REUSEADDR on IPv6 socket: %s\n", strerror(errno));
    }

    // Bind IPv6 socket to [::]:<port>
    memset(&addr6, 0, sizeof(addr6));
    addr6.sin6_family = AF_INET6;
    addr6.sin6_addr = in6addr_any; // ::
    addr6.sin6_port = htons(port);

    if (bind(ipv6_sock, (struct sockaddr *)&addr6, sizeof(addr6)) < 0)
    {
        printf("Error: Failed to bind IPv6 socket to [::]:%d: %s\n", port, strerror(errno));
        close(ipv6_sock);
        return -1;
    }

    if (listen(ipv6_sock, 5) < 0)
    {
        printf("Error: Failed to listen on IPv6 socket: %s\n", strerror(errno));
        close(ipv6_sock);
        return -1;
    }

    printf("OK: IPv6 socket opened on [::]:%d\n", port);

    // Now try to create and bind IPv4 socket
    ipv4_sock = socket(AF_INET, SOCK_STREAM, 0);
    if (ipv4_sock < 0)
    {
        printf("Error: Failed to create IPv4 socket: %s\n", strerror(errno));
        close(ipv6_sock);
        return -1;
    }

    if (setsockopt(ipv4_sock, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse)) < 0)
    {
        printf("Warning: Failed to set SO_REUSEADDR on IPv4 socket: %s\n", strerror(errno));
    }

    // Bind IPv4 socket to 0.0.0.0:<port>
    memset(&addr4, 0, sizeof(addr4));
    addr4.sin_family = AF_INET;
    addr4.sin_addr.s_addr = INADDR_ANY; // 0.0.0.0
    addr4.sin_port = htons(port);

    if (bind(ipv4_sock, (struct sockaddr *)&addr4, sizeof(addr4)) < 0)
    {
        if (errno == EADDRINUSE)
        {
            printf("Error: Could not open IPv4 socket, address is already in use.\n");
            printf("Result: Conflict! Your system's default is likely a dual-stack socket (IPV6_V6ONLY = 0).\n");
            result = 1; // Indicates dual-stack behavior
        }
        else
        {
            printf("Error: Failed to bind IPv4 socket: %s\n", strerror(errno));
            result = -1;
        }
    }
    else
    {
        if (listen(ipv4_sock, 5) < 0)
        {
            printf("Error: Failed to listen on IPv4 socket: %s\n", strerror(errno));
            result = -1;
        }
        else
        {
            printf("OK: IPv4 socket also opened on 0.0.0.0:%d\n", port);
            printf("Result: Success! Your system allows separate IPv4 and IPv6 binds by default.\n");
            result = 0; // Indicates separate socket behavior
        }
    }

    // Cleanup
    if (ipv4_sock >= 0)
        close(ipv4_sock);
    if (ipv6_sock >= 0)
        close(ipv6_sock);

    return result;
}

/**
 * Test 2: Explicitly sets IPV6_V6ONLY to 1 to ensure sockets can coexist.
 */
int run_v6only_test(int port)
{
    int ipv6_sock = -1, ipv4_sock = -1;
    struct sockaddr_in6 addr6;
    struct sockaddr_in addr4;
    int result = 0;

    printf("\n=== Test 2: Forcing Sockets with IPV6_V6ONLY ===\n");

    // Create IPv6 socket
    ipv6_sock = socket(AF_INET6, SOCK_STREAM, 0);
    if (ipv6_sock < 0)
    {
        printf("Error: Failed to create IPv6 socket: %s\n", strerror(errno));
        return -1;
    }

    // Set SO_REUSEADDR
    int reuse = 1;
    if (setsockopt(ipv6_sock, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse)) < 0)
    {
        printf("Warning: Failed to set SO_REUSEADDR on IPv6 socket: %s\n", strerror(errno));
    }

    // Explicitly set IPV6_V6ONLY to 1 (IPv6-only, no dual-stack)
    int v6only = 1;
    if (setsockopt(ipv6_sock, IPPROTO_IPV6, IPV6_V6ONLY, &v6only, sizeof(v6only)) < 0)
    {
        printf("Error: Failed to set IPV6_V6ONLY: %s\n", strerror(errno));
        close(ipv6_sock);
        return -1;
    }

    // Bind IPv6 socket to [::]:<port>
    memset(&addr6, 0, sizeof(addr6));
    addr6.sin6_family = AF_INET6;
    addr6.sin6_addr = in6addr_any; // ::
    addr6.sin6_port = htons(port);

    if (bind(ipv6_sock, (struct sockaddr *)&addr6, sizeof(addr6)) < 0)
    {
        printf("Error: Failed to bind IPv6 socket to [::]:%d: %s\n", port, strerror(errno));
        close(ipv6_sock);
        return -1;
    }

    if (listen(ipv6_sock, 5) < 0)
    {
        printf("Error: Failed to listen on IPv6 socket: %s\n", strerror(errno));
        close(ipv6_sock);
        return -1;
    }

    printf("OK: IPv6 socket with 'IPV6_V6ONLY' opened on [::]:%d\n", port);

    // Now try to create and bind IPv4 socket
    ipv4_sock = socket(AF_INET, SOCK_STREAM, 0);
    if (ipv4_sock < 0)
    {
        printf("Error: Failed to create IPv4 socket: %s\n", strerror(errno));
        close(ipv6_sock);
        return -1;
    }

    if (setsockopt(ipv4_sock, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse)) < 0)
    {
        printf("Warning: Failed to set SO_REUSEADDR on IPv4 socket: %s\n", strerror(errno));
    }

    // Bind IPv4 socket to 0.0.0.0:<port>
    memset(&addr4, 0, sizeof(addr4));
    addr4.sin_family = AF_INET;
    addr4.sin_addr.s_addr = INADDR_ANY; // 0.0.0.0
    addr4.sin_port = htons(port);

    if (bind(ipv4_sock, (struct sockaddr *)&addr4, sizeof(addr4)) < 0)
    {
        if (errno == EADDRINUSE)
        {
            printf("Error: Could not open IPv4 socket, address is already in use.\n");
            printf("Result: Unexpected! Even with IPV6_V6ONLY=1, there's still a conflict.\n");
            result = 1;
        }
        else
        {
            printf("Error: Failed to bind IPv4 socket: %s\n", strerror(errno));
            result = -1;
        }
    }
    else
    {
        if (listen(ipv4_sock, 5) < 0)
        {
            printf("Error: Failed to listen on IPv4 socket: %s\n", strerror(errno));
            result = -1;
        }
        else
        {
            printf("OK: IPv4 socket also opened on 0.0.0.0:%d\n", port);
            printf("Result: Success! IPV6_V6ONLY=1 allows separate IPv4 and IPv6 sockets.\n");
            result = 0;
        }
    }

    // Cleanup
    if (ipv4_sock >= 0)
        close(ipv4_sock);
    if (ipv6_sock >= 0)
        close(ipv6_sock);

    return result;
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s <port>\n", argv[0]);
        printf("Example: %s 9876\n", argv[0]);
        return 1;
    }

    int port = atoi(argv[1]);
    if (port <= 0 || port > 65535)
    {
        printf("Error: Invalid port number. Must be between 1 and 65535.\n");
        return 1;
    }

    printf("Starting socket test with port: %d\n", port);

    // Run both tests
    int result1 = run_default_test(port);
    int result2 = run_v6only_test(port);

    printf("\n=== Summary ===\n");
    printf("Test 1 (Default): %s\n",
           result1 == 0 ? "Separate sockets work" : result1 == 1 ? "Dual-stack conflict detected"
                                                                 : "Error occurred");
    printf("Test 2 (V6ONLY): %s\n",
           result2 == 0 ? "Separate sockets work" : result2 == 1 ? "Unexpected conflict"
                                                                 : "Error occurred");

    return 0;
}
