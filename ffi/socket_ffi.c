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
#define ADDRESS_LEN 16

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

uint32_t *socketGetHostByName(uint32_t context_len, uint32_t *hPtr )
{
  char hostname[HOSTNAME_LEN];
  uint32_t hostname_len;
  struct addrinfo hints, *res;
  char buf[HOSTNAME_LEN] = "";
  int error;

  unboxString( hPtr, hostname, HOSTNAME_LEN, &hostname_len );
  hostname[hostname_len] = (char) 0;

  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_INET;

  error = getaddrinfo(hostname, NULL, &hints, &res);
  if(error != 0)
    {
      fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(error));
      exit(1);
    }

  error = getnameinfo(res->ai_addr, res->ai_addrlen, buf, sizeof(buf), NULL, 0, NI_NUMERICHOST);
  if(error != 0)
    {
      fprintf(stderr, "getnameinfo error: %s\n", gai_strerror(error));
      exit(1);
    }

  freeaddrinfo(res);

  return boxString( context_len,
                    buf,
                    strlen(buf) );
}

uint32_t *socketConnect(uint32_t context_len, uint32_t *tuple )
{
  int sd;
  char hostname[HOSTNAME_LEN];
  uint32_t hostname_len;
  int port;
  int status;
  struct sockaddr_in serverName = { 0 };
  struct addrinfo hints, *res, *r;
  int error;

  sd = (int) unboxUnsigned( unboxTuple(tuple, 0) );

  unboxString( unboxTuple(tuple, 1), hostname, HOSTNAME_LEN, &hostname_len );
  hostname[hostname_len] = (char) 0;

  port = (int) unboxUnsigned( unboxTuple(tuple, 2) );

  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_INET;

  error = getaddrinfo(hostname, NULL, &hints, &res);
  if(error != 0)
    {
      fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(error));
      exit(1);
    }

  for ( r=res; r != NULL; r=r->ai_next) 
    {
      memset(&serverName, 0x0, sizeof(serverName));
      memcpy(&serverName, r->ai_addr, r->ai_addrlen);
      serverName.sin_port = htons(port);

      status = connect(sd, 
                       (struct sockaddr *) &serverName,
                       sizeof(serverName));

      if (status == 0)
        {
          break;
        }
    }

  freeaddrinfo(res);

  if (r == NULL) 
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
  char hostname[HOSTNAME_LEN];
  uint32_t hostname_len;
  int status = 0;
  struct sockaddr_in serverName = { 0 };
  struct addrinfo hints, *res, *r;
  int error;

  sd = (int) unboxUnsigned( unboxTuple(tuple, 0) );

  unboxString( unboxTuple(tuple, 1), hostname, HOSTNAME_LEN, &hostname_len );
  hostname[hostname_len] = (char) 0;

  port = (int) unboxUnsigned( unboxTuple(tuple, 2) );

  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_INET;

  error = getaddrinfo(hostname, NULL, &hints, &res);
  if(error != 0)
    {
      fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(error));
      exit(1);
    }

  for ( r=res; r != NULL; r=r->ai_next) 
    {
      memset(&serverName, 0x0, sizeof(serverName));
      memcpy(&serverName, r->ai_addr, r->ai_addrlen);
      serverName.sin_port = htons(port);
      //serverName.sin_addr.s_addr=htonl(INADDR_ANY);
      //serverName.sin_family = AF_INET;

      status = bind(sd, 
		    (struct sockaddr *) &serverName,
		    sizeof(serverName));

      if (status == 0)
        {
          break;
        }
    }

  freeaddrinfo(res);

  if (r == NULL) 
    {
      perror("bind:");
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
