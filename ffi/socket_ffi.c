/*
 * socket_ffi.c
 *
 * Some code taken from Ivan Griffin (ivan.griffin@ul.ie)
 *
 */

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
#include <fcntl.h>

#include "ffi.h"

#define HOSTNAME_LEN 256

uint32_t *socketOpenTCP( uint32_t context_len, uint32_t *stackvar )
{
  int sd;
  int status;
  int enabled = 1;

  sd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (-1 == sd)
  {
    perror("socket:");
    exit(1);
  }

  status = setsockopt(sd, 
                      SOL_SOCKET,
                      SO_REUSEADDR,
                      (const char *) &enabled, 
                      sizeof(enabled));

  if (-1 == status)
  {
    perror("setsockopt(...,SO_REUSEADDR,...)");
  }
 
  /* SUSP -- add the NODELAY one too */

  return boxUnsigned( context_len, (uint32_t) sd);
}

uint32_t *socketOpenUDP( uint32_t context_len, uint32_t *stackvar )
{
  int sd;
  int status;
  int enabled = 1;

  sd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if (-1 == sd)
  {
    perror("socket:");
    exit(1);
  }

  status = setsockopt(sd, 
                      SOL_SOCKET,
                      SO_REUSEADDR,
                      (const char *) &enabled, 
                      sizeof(enabled));

  if (-1 == status)
  {
    perror("setsockopt(...,SO_REUSEADDR,...)");
  }
 
  /* SUSP -- add the NODELAY one too */

  return boxUnsigned( context_len, (uint32_t) sd);
}

uint32_t *socketSetNonBlocking(uint32_t context_len, uint32_t *hPtr)
{
  int sd;
  int flags;

  sd = (int) unboxUnsigned( hPtr );

  flags = fcntl(sd, F_GETFL, 0);
  assert( flags >= 0 );

  fcntl(sd, F_SETFL, flags | O_NONBLOCK);

  return (uint32_t *) NULL;
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

  unboxString( unboxTuple(tuple, 1), hostname, HOSTNAME_LEN );

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

uint32_t *socketBind(uint32_t context_len, uint32_t *tuple )
{
  int sd;
  int port;
  int status = 0;
  struct hostent *hostPtr = NULL;
  struct sockaddr_in serverName = { 0 };

  sd = (int) unboxUnsigned( unboxTuple(tuple, 0) );
  port = (int) unboxUnsigned( unboxTuple(tuple, 1) );

  hostPtr = gethostbyname("127.0.0.1");
  if (NULL == hostPtr)
  {
    perror("gethostbyname()");
    exit(1);
  }

  (void) memset(&serverName, 0,
                sizeof(serverName));
  (void) memcpy(&serverName.sin_addr,
                hostPtr->h_addr,
                hostPtr->h_length);

  serverName.sin_addr.s_addr=htonl(INADDR_ANY);
  serverName.sin_family = AF_INET;
  serverName.sin_port = htons(port);

  status = bind(sd,
                (struct sockaddr *) &serverName,
                sizeof(serverName));
  if (-1 == status)
  {
    perror("bind()");
    exit(1);
  }

  return (uint32_t *) NULL;
}

uint32_t *socketListen(uint32_t context_len, uint32_t *tuple )
{
  int sd;
  int n;
  int status;

  sd = (int) unboxUnsigned( unboxTuple(tuple, 0) );
  n = (int) unboxUnsigned( unboxTuple(tuple, 1) );

  status = listen(sd, n);
  if (-1 == status)
  {
    perror("listen()");
    exit(1);
  }

  return (uint32_t *) NULL;
}

uint32_t *socketAccept(uint32_t context_len, uint32_t *hptr )
{
  int sd;
  int new_sd;
  struct sockaddr_in clientName = { 0 };
  int clientLength = sizeof(clientName);

  sd = (int) unboxUnsigned( hptr );

  (void) memset(&clientName, 0,
                sizeof(clientName));

  new_sd = accept(sd,
                  (struct sockaddr *) &clientName,
                  (uint32_t *) &clientLength);

  if (-1 == new_sd)
  {
    perror("accept()");
    exit(1);
  }

  return boxUnsigned( context_len, new_sd );
}
