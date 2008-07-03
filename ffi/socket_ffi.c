#include <stdio.h>                /* perror() */
#include <stdlib.h>               /* atoi() */
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>               /* read() */
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <assert.h>
#include <inttypes.h>

#include "ffi.h"

#define HOSTNAME_LEN 256

uint32_t *socketOpen( uint32_t context_len, uint32_t *stackvar )
{
  int sd;

  sd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (-1 == sd)
  {
    perror("socket:");
    exit(1);
  }
 
  return boxUnsigned( context_len, (uint32_t) sd);
}

uint32_t *socketConnect(uint32_t context_len, uint32_t *tuple )
{
  int sd;
  char hostname[HOSTNAME_LEN];
  int port;
  int status;
  struct hostent *hostPtr = NULL;
  struct sockaddr_in serverName = { 0 };

  sd = (int) unboxUnsigned( unboxTuple(tuple, 0) );

  unboxString( unboxTuple(tuple, 1), hostname, sizeof(hostname) );

  port = (int) unboxUnsigned( unboxTuple(tuple, 2) );

  hostPtr = gethostbyname(hostname);
  if (NULL == hostPtr)
  {
    hostPtr = gethostbyaddr(hostname, 
                            strnlen(hostname, sizeof(hostname)), 
                            AF_INET);
    if (NULL == hostPtr)
    {
      perror("Error resolving server address:");
      exit(1);
    }
  }

  serverName.sin_family = AF_INET;
  serverName.sin_port = htons(port);

  memcpy(&serverName.sin_addr,
         hostPtr->h_addr,
         hostPtr->h_length);

  status = connect(sd, 
                   (struct sockaddr *) &serverName,
                   sizeof(serverName));

  if (-1 == status)
  {
    perror("connect:");
    exit(1);
  }

  return (uint32_t *) NULL;
}

