// Sample: I/O Completion Port IPv4/IPv6 Server
//
// Files:
//      IOComplePortServersrc.cpp    - this file
//      resolve.cpp       - Common name resolution routines
//      resolve.h         - Header file for name resolution routines
//
// Description:
//      This sample illustrates how to write a scalable, high-performance
//      Winsock server. This is implemented as a TCP (IPv4/IPv6) server
//      designed to handle many connections simultaneously. The purpose
//      of this server is an echo server. For each accepted connection,
//      data is read and then sent back to the client. Several limitations
//      are present to ensure that as many concurrent connections can
//      be handled. First, each connection has only a single overlapped
//      receive posted at any given time. Second, each connection may only
//      have a maximum of five outstanding overlapped sends. This prevents
//      a malicious client from connecting and only sending data -- this
//      would cause an unlimited number of data to be buffered since the
//      sends would block as the client is not receiving data (and the TCP
//      window size goes to zero).
//
//      This sample illustrates overlapped IO with a completion port for
//      TCP over both IPv4 and IPv6. This sample uses the
//      getaddrinfo/getnameinfo APIs which allows this application to be
//      IP version independent. That is the desired address family
//      (AF_INET or AF_INET6) can be determined simply from the string
//      address passed via the -l command.
//
//      For TCP, a listening socket is created for each IP address family
//      available. Each socket is associated with a completion port and
//      worker threads are spawned (one for each CPU available). For each
//      listening thread, a number of AcceptEx are posted. The worker threads
//      then wait for one of these to complete. Upon completion, the client
//      socket is associated with the completion port and several receives
//      are posted. The AcceptEx is reposted as well. Once data is received
//      on a client socket, it is echoed back.
//
//      The important thing to remember with IOCP is that the completion events
//      may occur out of order; however, the buffers are guaranteed to be filled
//      in the order posted. For our echo server this can cause problems as
//      receive N+1 may complete before receive N. We can't echo back N+1 before
//      echoing N. There are two approaches possible. First, we could surmise
//      that since receive N+1 has completed then we can safely echo back receive
//      N and N+1 at that time (to maintain the data ordering). To do this properly
//      you'll have to call WSAGetOverlappedResult on receive N in order to find
//      out how many bytes were received to echo it back. The second approach
//      (which is implemented in this sample) is to keep a list of receive
//      buffers that completed out of order. This list is maintained in the
//      per-socket data structure. When receive N+1 completes, it will notice that
//      receive N has not completed. The buffer is then queued in the out of
//      order send list. Once receive N completes, its buffer is queued -- the
//      queue is ordered in the same order that the receive operations are.
//      Another routine (DoSends) goes through this list and sends those buffers
//      that are available and in order. If any gaps are detected no further buffers
//      are sent (as we will wait for that receive to complete and insert its
//      buffer into the list so that the next call to DoSends will correctly
//      send the buffers in the right order).
//
//      For example:
//          If this sample is called with the following command lines:
//              IOComplePortServer -l fe80::2efe:1234 -e 5150
//              IOComplePortServer -l ::
//          Then the server creates an IPv6 socket as an IPv6 address was provided.
//
//          On the other hand, with the following command line:
//              IOComplePortServer -l 7.7.7.1 -e 5150
//              IOComplePortServer -l 0.0.0.0
//          Then the server creates an IPv4 socket.
//
//          Calling the server with no parameters will create a server that
//          listens both IPv4 and IPv6 (if installed).
// Usage:
//      IOComplePortServer [options]
//          -a 4|6     Address family, 4 = IPv4, 6 = IPv6 [default = IPv4]
//          -b size    Buffer size for send/recv
//          -e port    Port number
//          -l addr    Local address to bind to [default INADDR_ANY for IPv4 or INADDR6_ANY for IPv6]
//          -os count  Maximum number of overlapped send operations to allow simultaneously (per socket)
//          -oa count  Maximum number of overlapped accepts to allow simultaneously
//          -o  count  Number of initial overlapped accepts to post
//
// Link to ws2_32.lib
#include <winsock2.h>
#include <ws2tcpip.h>
// Link to Mswsock.lib, Microsoft specific
#include <mswsock.h>
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#define DEFAULT_BUFFER_SIZE         4096           // default buffer size
#define DEFAULT_OVERLAPPED_COUNT    5      // Number of overlapped recv per socket
#define MAX_OVERLAPPED_ACCEPTS      500
#define MAX_OVERLAPPED_SENDS        200
#define MAX_OVERLAPPED_RECVS        200
#define MAX_COMPLETION_THREAD_COUNT 32// Maximum number of completion threads allowed
#define BURST_ACCEPT_COUNT          100
// Common routines for resolving addresses and hostnames
// Files:
//      resolve.h       - Header file for common routines
// Description:
//      This file contains common name resolution and name printing
//      routines and is used by many of the samples.
//
#ifndef _RESOLVE_H_
#define _RESOLVE_H_
#ifdef _cplusplus
extern "C" {
#endif
    int              PrintAddress(SOCKADDR *sa, int salen);
    int              FormatAddress(SOCKADDR *sa, int salen, char *addrbuf, int addrbuflen);
    struct addrinfo *ResolveAddress(char *addr, char *port, int af, int type, int proto);
#ifdef _cplusplus
}
#endif
#endif
int gAddressFamily = AF_UNSPEC,         // default to unspecified
    gSocketType    = SOCK_STREAM,       // default to TCP socket type
    gProtocol      = IPPROTO_TCP,              // default to TCP protocol
    gBufferSize    = DEFAULT_BUFFER_SIZE,
    gInitialAccepts = DEFAULT_OVERLAPPED_COUNT,
    gMaxAccepts    = MAX_OVERLAPPED_ACCEPTS,
    gMaxReceives   = MAX_OVERLAPPED_RECVS,
    gMaxSends      = MAX_OVERLAPPED_SENDS;
    char *gBindAddr    = NULL,         // local interface to bind to
    *gBindPort    = "5150";            // local port to bind to
    // Statistics counters
    volatile LONG gBytesRead=0, gBytesSent=0, gStartTime=0, gBytesReadLast=0, gBytesSentLast=0,
    gStartTimeLast=0, gConnections=0, gConnectionsLast=0, gOutstandingSends=0;
    // This is our per I/O buffer. It contains a WSAOVERLAPPED structure as well
    //    as other necessary information for handling an IO operation on a socket.
    typedef struct _BUFFER_OBJ
{
    WSAOVERLAPPED        ol;
    SOCKET               sclient;       // Used for AcceptEx client socket
    HANDLE               PostAccept;
    char                *buf;               // Buffer for recv/send/AcceptEx
    int                  buflen;            // Length of the buffer
    int                  operation;     // Type of operation issued
#define OP_ACCEPT       0                // AcceptEx
#define OP_READ         1                   // WSARecv/WSARecvFrom
#define OP_WRITE        2                   // WSASend/WSASendTo
    SOCKADDR_STORAGE     addr;
    int                  addrlen;
    struct _SOCKET_OBJ  *sock;
    struct _BUFFER_OBJ  *next;
} BUFFER_OBJ;
// This is our per socket buffer. It contains information about the socket handle
//    which is returned from each GetQueuedCompletionStatus call.
typedef struct _SOCKET_OBJ
{
    SOCKET    s;               // Socket handle
    int                af,              // Address family of socket (AF_INET, AF_INET6)
                       bClosing;        // Is the socket closing?
    volatile LONG      OutstandingRecv, // Number of outstanding overlapped ops on
             OutstandingSend, PendingSend;
    CRITICAL_SECTION   SockCritSec;     // Protect access to this structure
    struct _SOCKET_OBJ  *next;
} SOCKET_OBJ;
//
typedef struct _LISTEN_OBJ
{
    SOCKET          s;
    int             AddressFamily;
    BUFFER_OBJ     *PendingAccepts; // Pending AcceptEx buffers
    volatile long   PendingAcceptCount;
    int             HiWaterMark, LoWaterMark;
    HANDLE          AcceptEvent;
    HANDLE          RepostAccept;
    volatile long   RepostCount;
    // Pointers to Microsoft specific extensions.
    LPFN_ACCEPTEX             lpfnAcceptEx;
    LPFN_GETACCEPTEXSOCKADDRS lpfnGetAcceptExSockaddrs;
    CRITICAL_SECTION ListenCritSec;
    struct _LISTEN_OBJ *next;
} LISTEN_OBJ;
// Serialize access to the free lists below
CRITICAL_SECTION gBufferListCs, gSocketListCs, gPendingCritSec;
// Lookaside lists for free buffers and socket objects
BUFFER_OBJ *gFreeBufferList=NULL;
SOCKET_OBJ *gFreeSocketList=NULL;
BUFFER_OBJ *gPendingSendList=NULL, *gPendingSendListEnd=NULL;
int  PostSend(SOCKET_OBJ *sock, BUFFER_OBJ *sendobj);
int  PostRecv(SOCKET_OBJ *sock, BUFFER_OBJ *recvobj);
void FreeBufferObj(BUFFER_OBJ *obj);
// Function: usage
// Description: Prints usage information and exits the process.
int usage(char *progname)
{
    fprintf(stderr, "Usage: %s [-a 4|6] [-e port] [-l local-addr] [-p udp|tcp]\n", progname);
    fprintf(stderr, "  -a  4|6     Address family, 4 = IPv4, 6 = IPv6 [default = IPv4]\n"
            "                                      else will listen to both IPv4 and IPv6\n"
            "  -b  size    Buffer size for send/recv [default = %d]\n"
            "  -e  port    Port number [default = %s]\n"
            "  -l  addr    Local address to bind to [default INADDR_ANY for IPv4 or INADDR6_ANY for IPv6]\n"
            "  -oa count   Maximum overlapped accepts to allow\n"
            "  -os count   Maximum overlapped sends to allow\n"
            "  -or count   Maximum overlapped receives to allow\n"
            "  -o  count   Initial number of overlapped accepts to post\n",
            gBufferSize,
            gBindPort
                );
    return 0;
}
// Function: dbgprint
// Description: Prints a message if compiled with the DEBUG flag.
void dbgprint(char *format,...)
{
#ifdef DEBUG
    va_list vl;
    char    dbgbuf[2048];
    va_start(vl, format);
    wvsprintf(dbgbuf, format, vl);
    va_end(vl);
    printf(dbgbuf);
    OutputDebugString(dbgbuf);
#endif
}
// Function: EnqueuePendingOperation
// Description: Enqueues a buffer object into a list (at the end).
void EnqueuePendingOperation(BUFFER_OBJ **head, BUFFER_OBJ **end, BUFFER_OBJ *obj, int op)
{
    EnterCriticalSection(&gPendingCritSec);
    if (op == OP_READ)
        ;
    else if (op == OP_WRITE)
        InterlockedIncrement(&obj->sock->PendingSend);
    obj->next = NULL;
    if (*end)
    {
        (*end)->next = obj;
        (*end) = obj;
    }
    else
    {
        (*head) = (*end) = obj;
    }
    LeaveCriticalSection(&gPendingCritSec);
    return;
}
// Function: DequeuePendingOperation
// Description: Dequeues the first entry in the list.
BUFFER_OBJ *DequeuePendingOperation(BUFFER_OBJ **head, BUFFER_OBJ **end, int op)
{
    BUFFER_OBJ *obj=NULL;
    EnterCriticalSection(&gPendingCritSec);
    if (*head)
    {
        obj = *head;
        (*head) = obj->next;
        // If next is NULL, no more objects are in the queue
        if (obj->next == NULL)
        {
            (*end) = NULL;
        }
        if (op == OP_READ)
            ;
        else if (op == OP_WRITE)
            InterlockedDecrement(&obj->sock->PendingSend);
    }
    LeaveCriticalSection(&gPendingCritSec);
    return obj;
}
// Function: ProcessPendingOperations
// Description:
//    This function goes through the list of pending send operations and posts them
//    as long as the maximum number of outstanding sends is not exceeded.
void ProcessPendingOperations()
{
    BUFFER_OBJ *sendobj=NULL;
    while(gOutstandingSends < gMaxSends)
    {
        sendobj = DequeuePendingOperation(&gPendingSendList, &gPendingSendListEnd, OP_WRITE);
        if (sendobj)
        {
            if (PostSend(sendobj->sock, sendobj) == SOCKET_ERROR)
            {
                // Cleanup
                printf("ProcessPendingOperations: PostSend failed!\n");
                FreeBufferObj(sendobj);
                break;
            }
        }
        else
        {
            break;
        }
    }
    return;
}
// Function: InsertPendingAccept
// Description: Inserts a pending accept operation into the listening object.
void InsertPendingAccept(LISTEN_OBJ *listenobj, BUFFER_OBJ *obj)
{
    obj->next = NULL;
    EnterCriticalSection(&listenobj->ListenCritSec);
    if (listenobj->PendingAccepts == NULL)
    {
        listenobj->PendingAccepts = obj;
    }
    else
    {
        // Insert at head - order doesn't really matter
        obj->next = listenobj->PendingAccepts;
        listenobj->PendingAccepts = obj;
    }
    LeaveCriticalSection(&listenobj->ListenCritSec);
}
// Function: RemovePendingAccept
// Description:
//    Removes the indicated accept buffer object from the list of pending
//    accepts in the listening object.
void RemovePendingAccept(LISTEN_OBJ *listenobj, BUFFER_OBJ *obj)
{
    BUFFER_OBJ *ptr=NULL, *prev=NULL;
    EnterCriticalSection(&listenobj->ListenCritSec);
    // Search list until we find the object
    ptr = listenobj->PendingAccepts;
    while ( (ptr) && (ptr != obj) )
    {
        prev = ptr;
        ptr  = ptr->next;
    }
    if (prev)
    {
        // Object is somewhere after the first entry
        prev->next = obj->next;
    }
    else
    {
        // Object is the first entry
        listenobj->PendingAccepts = obj->next;
    }
    LeaveCriticalSection(&listenobj->ListenCritSec);
}
// Function: GetBufferObj
// Description:
//    Allocate a BUFFER_OBJ. A look aside list is maintained to increase performance
//    as these objects are allocated frequently.
BUFFER_OBJ *GetBufferObj(int buflen)
{
    BUFFER_OBJ *newobj=NULL;
    EnterCriticalSection(&gBufferListCs);
    if (gFreeBufferList == NULL)
    {
        // Allocate the object
        newobj = (BUFFER_OBJ *)HeapAlloc(
                GetProcessHeap(),
                HEAP_ZERO_MEMORY,
                sizeof(BUFFER_OBJ) + (sizeof(BYTE) * buflen)
                );
        if (newobj == NULL)
        {
            fprintf(stderr, "GetBufferObj: HeapAlloc failed: %d\n", GetLastError());
        }
    }
    else
    {
        newobj          = gFreeBufferList;
        gFreeBufferList = newobj->next;
        newobj->next    = NULL;
    }
    LeaveCriticalSection(&gBufferListCs);
    if (newobj)
    {
        newobj->buf     = (char *)(((char *)newobj) + sizeof(BUFFER_OBJ));
        newobj->buflen  = buflen;
        newobj->addrlen = sizeof(newobj->addr);
    }
    return newobj;
}
// Function: FreeBufferObj
// Description: Free the buffer object. This adds the object to the free look aside list.
void FreeBufferObj(BUFFER_OBJ *obj)
{
    EnterCriticalSection(&gBufferListCs);
    memset(obj, 0, sizeof(BUFFER_OBJ) + gBufferSize);
    obj->next = gFreeBufferList;
    gFreeBufferList = obj;
    LeaveCriticalSection(&gBufferListCs);
}
// Function: GetSocketObj
// Description:
//    Allocate a socket object and initialize its members. A socket object is
//    allocated for each socket created (either by socket or accept).
//    Socket objects are returned from a look aside list if available. Otherwise, a new object is allocated.
SOCKET_OBJ *GetSocketObj(SOCKET s, int af)
{
    SOCKET_OBJ  *sockobj=NULL;
    EnterCriticalSection(&gSocketListCs);
    if (gFreeSocketList == NULL)
    {
        sockobj = (SOCKET_OBJ *)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(SOCKET_OBJ));
        if (sockobj == NULL)
        {
            fprintf(stderr, "GetSocketObj: HeapAlloc failed: %d\n", GetLastError());
        }
        else
        {
            InitializeCriticalSection(&sockobj->SockCritSec);
        }
    }
    else
    {
        sockobj         = gFreeSocketList;
        gFreeSocketList = sockobj->next;
        sockobj->next   = NULL;
    }
    LeaveCriticalSection(&gSocketListCs);
    // Initialize the members
    if (sockobj)
    {
        sockobj->s  = s;
        sockobj->af = af;
    }
    return sockobj;
}
// Function: FreeSocketObj
// Description: Frees a socket object. The object is added to the lookaside list.
void FreeSocketObj(SOCKET_OBJ *obj)
{
    CRITICAL_SECTION cstmp;
    BUFFER_OBJ      *ptr=NULL;
    // Close the socket if it hasn't already been closed
    if (obj->s != INVALID_SOCKET)
    {
        printf("FreeSocketObj: closing socket\n");
        closesocket(obj->s);
        obj->s = INVALID_SOCKET;
    }
    EnterCriticalSection(&gSocketListCs);
    cstmp = obj->SockCritSec;
    memset(obj, 0, sizeof(SOCKET_OBJ));
    obj->SockCritSec = cstmp;
    obj->next = gFreeSocketList;
    gFreeSocketList = obj;
    LeaveCriticalSection(&gSocketListCs);
}
// Function: ValidateArgs
// Description: Parses the command line arguments and sets up some global variables.
void ValidateArgs(int argc, char **argv)
{
    int     i;
    for(i=1; i < argc ;i++)
    {
        if (((argv[i][0] != '/') && (argv[i][0] != '-')) || (strlen(argv[i]) < 2))
            usage(argv[0]);
        else
        {
            switch (tolower(argv[i][1]))
            {
                case 'a':               // address family - IPv4 or IPv6
                    if (i+1 >= argc)
                        usage(argv[0]);
                    if (argv[i+1][0] == '4')
                        gAddressFamily = AF_INET;
                    else if (argv[i+1][0] == '6')
                        gAddressFamily = AF_INET6;
                    else
                        usage(argv[0]);
                    i++;
                    break;
                case 'b':               // buffer size for send/recv
                    if (i+1 >= argc)
                        usage(argv[0]);
                    gBufferSize = atol(argv[++i]);
                    break;
                case 'e':               // endpoint - port number
                    if (i+1 >= argc)
                        usage(argv[0]);
                    gBindPort = argv[++i];
                    break;
                case 'l':               // local address for binding
                    if (i+1 >= argc)
                        usage(argv[0]);
                    gBindAddr = argv[++i];
                    break;
                case 'o':               // overlapped count
                    if (i+1 >= argc)
                        usage(argv[0]);
                    if (strlen(argv[i]) == 2)       // Overlapped accept initial count
                    {
                        gInitialAccepts = atol(argv[++i]);
                    }
                    else if (strlen(argv[i]) == 3)
                    {
                        if (tolower(argv[i][2]) == 'a')
                            gMaxAccepts = atol(argv[++i]);
                        else if (tolower(argv[i][2]) == 's')
                            gMaxSends = atol(argv[++i]);
                        else if (tolower(argv[i][2]) == 'r')
                            gMaxReceives = atol(argv[++i]);
                        else
                            usage(argv[0]);
                    }
                    else
                    {
                        usage(argv[0]);
                    }
                    break;
                default:
                    usage(argv[0]);
                    break;
            }
        }
    }
}
// Function: PrintStatistics
// Description: Print the send/recv statistics for the server
void PrintStatistics()
{
    ULONG       bps, tick, elapsed, cps;
    tick = GetTickCount();
    elapsed = (tick - gStartTime) / 1000;
    if (elapsed == 0)
        return;
    printf("\n");
    // Calculate average bytes per second
    bps = gBytesSent / elapsed;
    printf("Average BPS sent: %lu [%lu]\n", bps, gBytesSent);
    bps = gBytesRead / elapsed;
    printf("Average BPS read: %lu [%lu]\n", bps, gBytesRead);
    elapsed = (tick - gStartTimeLast) / 1000;
    if (elapsed == 0)
        return;
    // Calculate bytes per second over the last X seconds
    bps = gBytesSentLast / elapsed;
    printf("Current BPS sent: %lu\n", bps);
    bps = gBytesReadLast / elapsed;
    printf("Current BPS read: %lu\n", bps);
    cps = gConnectionsLast / elapsed;
    printf("Current conns/sec: %lu\n", cps);
    printf("Total connections: %lu\n", gConnections);
    InterlockedExchange(&gBytesSentLast, 0);
    InterlockedExchange(&gBytesReadLast, 0);
    InterlockedExchange(&gConnectionsLast, 0);
    gStartTimeLast = tick;
}
// Function: PostRecv
// Description: Post an overlapped receive operation on the socket.
int PostRecv(SOCKET_OBJ *sock, BUFFER_OBJ *recvobj)
{
    WSABUF  wbuf;
    DWORD   bytes, flags;
    int     rc;
    recvobj->operation = OP_READ;
    wbuf.buf = recvobj->buf;
    wbuf.len = recvobj->buflen;
    flags = 0;
    EnterCriticalSection(&sock->SockCritSec);
    rc = WSARecv(sock->s, &wbuf, 1, &bytes, &flags, &recvobj->ol, NULL);
    if (rc == SOCKET_ERROR)
    {
        rc = NO_ERROR;
        if (WSAGetLastError() != WSA_IO_PENDING)
        {
            dbgprint("PostRecv: WSARecv* failed: %d\n", WSAGetLastError());
            rc = SOCKET_ERROR;
        }
    }
    if (rc == NO_ERROR)
    {
        // Increment outstanding overlapped operations
        InterlockedIncrement(&sock->OutstandingRecv);
    }
    LeaveCriticalSection(&sock->SockCritSec);
    return rc;
}
// Function: PostSend
// Description: Post an overlapped send operation on the socket.
int PostSend(SOCKET_OBJ *sock, BUFFER_OBJ *sendobj)
{
    WSABUF  wbuf;
    DWORD   bytes;
    int     rc, err;
    sendobj->operation = OP_WRITE;
    wbuf.buf = sendobj->buf;
    wbuf.len = sendobj->buflen;
    EnterCriticalSection(&sock->SockCritSec);
    rc = WSASend(sock->s, &wbuf, 1, &bytes, 0, &sendobj->ol, NULL);
    if (rc == SOCKET_ERROR)
    {
        rc = NO_ERROR;
        if ((err = WSAGetLastError()) != WSA_IO_PENDING)
        {
            if (err == WSAENOBUFS)
                DebugBreak();
            dbgprint("PostSend: WSASend* failed: %d [internal = %d]\n", WSAGetLastError(), sendobj->ol.Internal);
            rc = SOCKET_ERROR;
        }
    }
    if (rc == NO_ERROR)
    {
        // Increment the outstanding operation count
        InterlockedIncrement(&sock->OutstandingSend);
        InterlockedIncrement(&gOutstandingSends);
    }    LeaveCriticalSection(&sock->SockCritSec);
    return rc;
}
// Function: PostAccept
// Description: Post an overlapped accept on a listening socket.
int PostAccept(LISTEN_OBJ *listen, BUFFER_OBJ *acceptobj)
{
    DWORD   bytes;
    int     rc;
    acceptobj->operation = OP_ACCEPT;
    // Create the client socket for an incoming connection
    acceptobj->sclient = socket(listen->AddressFamily, SOCK_STREAM, IPPROTO_TCP);
    if (acceptobj->sclient == INVALID_SOCKET)
    {
        fprintf(stderr, "PostAccept: socket failed: %d\n", WSAGetLastError());
        return -1;
    }
    rc = listen->lpfnAcceptEx(
            listen->s,
            acceptobj->sclient,
            acceptobj->buf,
            acceptobj->buflen - ((sizeof(SOCKADDR_STORAGE) + 16) * 2),
            sizeof(SOCKADDR_STORAGE) + 16,
            sizeof(SOCKADDR_STORAGE) + 16,
            &bytes,
            &acceptobj->ol
            );
    if (rc == FALSE)
    {
        if (WSAGetLastError() != WSA_IO_PENDING)
        {
            printf("PostAccept: AcceptEx failed: %d\n", WSAGetLastError());
            return SOCKET_ERROR;
        }
    }
    // Increment the outstanding overlapped count for this socket
    InterlockedIncrement(&listen->PendingAcceptCount);
    return NO_ERROR;
}
// Function: HandleIo
// Description:
//    This function handles the IO on a socket. In the event of a receive, the
//    completed receive is posted again. For completed accepts, another AcceptEx
//    is posted. For completed sends, the buffer is freed.
void HandleIo(ULONG_PTR key, BUFFER_OBJ *buf, HANDLE CompPort, DWORD BytesTransfered, DWORD error)
{
    LISTEN_OBJ *listenobj=NULL;
    SOCKET_OBJ *sockobj=NULL,
               *clientobj=NULL;                     // New client object for accepted connections
    BUFFER_OBJ *recvobj=NULL,       // Used to post new receives on accepted connections
               *sendobj=NULL;                      // Used to post new sends for data received
    BOOL        bCleanupSocket;
    if (error != 0)
    {
        dbgprint("OP = %d; Error = %d\n", buf->operation, error);
    }
    bCleanupSocket = FALSE;
    if (error != NO_ERROR)
    {
        // An error occurred on a TCP socket, free the associated per I/O buffer
        // and see if there are any more outstanding operations. If so we must
        // wait until they are complete as well.
        if (buf->operation != OP_ACCEPT)
        {
            sockobj = (SOCKET_OBJ *)key;
            if (buf->operation == OP_READ)
            {
                if ((InterlockedDecrement(&sockobj->OutstandingRecv) == 0) && (sockobj->OutstandingSend == 0) )
                {
                    dbgprint("Freeing socket obj in GetOverlappedResult\n");
                    FreeSocketObj(sockobj);
                }
            }
            else if (buf->operation == OP_WRITE)
            {
                if ((InterlockedDecrement(&sockobj->OutstandingSend) == 0) && (sockobj->OutstandingRecv == 0) )
                {
                    dbgprint("Freeing socket obj in GetOverlappedResult\n");
                    FreeSocketObj(sockobj);
                }
            }
        }
        else
        {
            listenobj = (LISTEN_OBJ *)key;
            printf("Accept failed\n");
            closesocket(buf->sclient);
            buf->sclient = INVALID_SOCKET;
        }
        FreeBufferObj(buf);
        return;
    }
    if (buf->operation == OP_ACCEPT)
    {
        HANDLE            hrc;
        SOCKADDR_STORAGE *LocalSockaddr=NULL, *RemoteSockaddr=NULL;
        int               LocalSockaddrLen,RemoteSockaddrLen;
        listenobj = (LISTEN_OBJ *)key;
        // Update counters
        InterlockedIncrement(&gConnections);
        InterlockedIncrement(&gConnectionsLast);
        InterlockedDecrement(&listenobj->PendingAcceptCount);
        InterlockedExchangeAdd(&gBytesRead, BytesTransfered);
        InterlockedExchangeAdd(&gBytesReadLast, BytesTransfered);
        // Print the client's addresses
        listenobj->lpfnGetAcceptExSockaddrs(
                buf->buf,
                buf->buflen - ((sizeof(SOCKADDR_STORAGE) + 16) * 2),
                sizeof(SOCKADDR_STORAGE) + 16,
                sizeof(SOCKADDR_STORAGE) + 16,
                (SOCKADDR **)&LocalSockaddr,
                &LocalSockaddrLen,
                (SOCKADDR **)&RemoteSockaddr,
                &RemoteSockaddrLen
                );
        RemovePendingAccept(listenobj, buf);
        // Get a new SOCKET_OBJ for the client connection
        clientobj = GetSocketObj(buf->sclient, listenobj->AddressFamily);
        if (clientobj)
        {
            // Associate the new connection to our completion port
            hrc = CreateIoCompletionPort((HANDLE)clientobj->s, CompPort, (ULONG_PTR)clientobj, 0);
            if (hrc == NULL)
            {
                fprintf(stderr, "CompletionThread: CreateIoCompletionPort failed: %d\n", GetLastError());
                return;
            }
            sendobj = buf;
            sendobj->buflen = BytesTransfered;
            // Post the send - this is the first one for this connection so just do it
            sendobj->sock = clientobj;
            // PostSend(clientobj, sendobj);
            EnqueuePendingOperation(&gPendingSendList, &gPendingSendListEnd, sendobj, OP_WRITE);
        }
        else
        {
            // Can't allocate a socket structure so close the connection
            closesocket(buf->sclient);
            buf->sclient = INVALID_SOCKET;
            FreeBufferObj(buf);
        }
        if (error != NO_ERROR)
        {
            // Check for socket closure
            EnterCriticalSection(&clientobj->SockCritSec);
            if ( (clientobj->OutstandingSend == 0) && (clientobj->OutstandingRecv == 0) )
            {
                closesocket(clientobj->s);
                clientobj->s = INVALID_SOCKET;
                FreeSocketObj(clientobj);
            }
            else
            {
                clientobj->bClosing = TRUE;
            }
            LeaveCriticalSection(&clientobj->SockCritSec);
            error = NO_ERROR;
        }
        InterlockedIncrement(&listenobj->RepostCount);
        SetEvent(listenobj->RepostAccept);
    }
    else if (buf->operation == OP_READ)
    {
        sockobj = (SOCKET_OBJ *)key;
        InterlockedDecrement(&sockobj->OutstandingRecv);
        // Receive completed successfully
        if (BytesTransfered > 0)
        {
            InterlockedExchangeAdd(&gBytesRead, BytesTransfered);
            InterlockedExchangeAdd(&gBytesReadLast, BytesTransfered);
            // Make the recv a send
            sendobj         = buf;
            sendobj->buflen = BytesTransfered;
            sendobj->sock = sockobj;
            //PostSend(sockobj, sendobj);
            EnqueuePendingOperation(&gPendingSendList, &gPendingSendListEnd, sendobj, OP_WRITE);
        }
        else
        {
            // dbgprint("Got 0 byte receive\n");
            // Graceful close - the receive returned 0 bytes read
            sockobj->bClosing = TRUE;
            // Free the receive buffer
            FreeBufferObj(buf);
            // If this was the last outstanding operation on socket, clean it up
            EnterCriticalSection(&sockobj->SockCritSec);
            if ((sockobj->OutstandingSend == 0) && (sockobj->OutstandingRecv == 0) )
            {
                bCleanupSocket = TRUE;
            }
            LeaveCriticalSection(&sockobj->SockCritSec);
        }
    }
    else if (buf->operation == OP_WRITE)
    {
        sockobj = (SOCKET_OBJ *)key;
        InterlockedDecrement(&sockobj->OutstandingSend);
        InterlockedDecrement(&gOutstandingSends);
        // Update the counters
        InterlockedExchangeAdd(&gBytesSent, BytesTransfered);
        InterlockedExchangeAdd(&gBytesSentLast, BytesTransfered);
        buf->buflen = gBufferSize;
        if (sockobj->bClosing == FALSE)
        {
            buf->sock = sockobj;
            PostRecv(sockobj, buf);
        }
    }
    ProcessPendingOperations();
    if (sockobj)
    {
        if (error != NO_ERROR)
        {
            printf("err = %d\n", error);
            sockobj->bClosing = TRUE;
        }
        // Check to see if socket is closing
        if ( (sockobj->OutstandingSend == 0) && (sockobj->OutstandingRecv == 0) && (sockobj->bClosing) )
        {
            bCleanupSocket = TRUE;
        }
        if (bCleanupSocket)
        {
            closesocket(sockobj->s);
            sockobj->s = INVALID_SOCKET;
            FreeSocketObj(sockobj);
        }
    }
    return;
}
// Function: CompletionThread
// Description:
//    This is the completion thread which services our completion port. One of
//    these threads is created per processor on the system. The thread sits in
//    an infinite loop calling GetQueuedCompletionStatus and handling socket IO that completed.
DWORD WINAPI CompletionThread(LPVOID lpParam)
{
    ULONG_PTR    Key;
    SOCKET       s;
    BUFFER_OBJ  *bufobj=NULL;                   // Per I/O object for completed I/O
    OVERLAPPED  *lpOverlapped=NULL;     // Pointer to overlapped structure for completed I/O
    HANDLE       CompletionPort;                    // Completion port handle
    DWORD        BytesTransfered,                   // Number of bytes transferred
                 Flags;                                     // Flags for completed I/O
    int          rc, error;
    CompletionPort = (HANDLE)lpParam;
    while (1)
    {
        error = NO_ERROR;
        rc = GetQueuedCompletionStatus(CompletionPort, &BytesTransfered, (PULONG_PTR)&Key, &lpOverlapped, INFINITE);
        bufobj = CONTAINING_RECORD(lpOverlapped, BUFFER_OBJ, ol);
        if (rc == FALSE)
        {
            // If the call fails, call WSAGetOverlappedResult to translate the
            //    error code into a Winsock error code.
            if (bufobj->operation == OP_ACCEPT)
            {
                s = ((LISTEN_OBJ *)Key)->s;
            }
            else
            {
                s = ((SOCKET_OBJ *)Key)->s;
            }
            dbgprint("CompletionThread: GetQueuedCompletionStatus failed: %d [0x%x]\n", GetLastError(), lpOverlapped->Internal);
            rc = WSAGetOverlappedResult(s, &bufobj->ol, &BytesTransfered, FALSE, &Flags);
            if (rc == FALSE)
            {
                error = WSAGetLastError();
            }
        }
        // Handle the IO operation
        HandleIo(Key, bufobj, CompletionPort, BytesTransfered, error);
    }
    ExitThread(0);
    return 0;
}
// Function: main
// Description:
//      This is the main program. It parses the command line and creates
//      the main socket. For TCP the socket is used to accept incoming
//      client connections. Each client TCP connection is handed off to
//      a worker thread which will receive any data on that connection
//      until the connection is closed.
int main(int argc, char **argv)
{
    WSADATA          wsd;
    SYSTEM_INFO      sysinfo;
    LISTEN_OBJ      *ListenSockets=NULL, *listenobj=NULL;
    SOCKET_OBJ      *sockobj=NULL;
    BUFFER_OBJ      *acceptobj=NULL;
    GUID             guidAcceptEx = WSAID_ACCEPTEX, guidGetAcceptExSockaddrs = WSAID_GETACCEPTEXSOCKADDRS;
    DWORD            bytes;
    HANDLE           CompletionPort, WaitEvents[MAX_COMPLETION_THREAD_COUNT], hrc;
    int              endpointcount=0, waitcount=0, interval, rc, i;
    struct addrinfo *res=NULL, *ptr=NULL;
    if(argc < 2)
    {
        usage(argv[0]);
        exit(1);
    }
    // Validate the command line
    ValidateArgs(argc, argv);
    // Load Winsock
    if (WSAStartup(MAKEWORD(2,2), &wsd) != 0)
    {
        fprintf(stderr, "unable to load Winsock!\n");
        return -1;
    }
    InitializeCriticalSection(&gSocketListCs);
    InitializeCriticalSection(&gBufferListCs);
    InitializeCriticalSection(&gPendingCritSec);
    // Create the completion port used by this server
    CompletionPort = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, (ULONG_PTR)NULL, 0);
    if (CompletionPort == NULL)
    {
        fprintf(stderr, "CreateIoCompletionPort failed: %d\n", GetLastError());
        return -1;
    }
    // Find out how many processors are on this system
    GetSystemInfo(&sysinfo);
    if (sysinfo.dwNumberOfProcessors > MAX_COMPLETION_THREAD_COUNT)
    {
        sysinfo.dwNumberOfProcessors = MAX_COMPLETION_THREAD_COUNT;
    }
    // Round the buffer size to the next increment of the page size
    if ((gBufferSize % sysinfo.dwPageSize) != 0)
    {
        gBufferSize = ((gBufferSize / sysinfo.dwPageSize) + 1) * sysinfo.dwPageSize;
    }
    printf("Buffer size = %lu (page size = %lu)\n", gBufferSize, sysinfo.dwPageSize);
    // Create the worker threads to service the completion notifications
    for(waitcount=0; waitcount < (int)sysinfo.dwNumberOfProcessors ;waitcount++)
    {
        WaitEvents[waitcount] = CreateThread(NULL, 0, CompletionThread, (LPVOID)CompletionPort, 0, NULL);
        if (WaitEvents[waitcount] == NULL)
        {
            fprintf(stderr, "CreatThread failed: %d\n", GetLastError());
            return -1;
        }
    }
    printf("Local address: %s; Port: %s; Family: %d\n", gBindAddr, gBindPort, gAddressFamily);
    // Obtain the "wildcard" addresses for all the available address families
    res = ResolveAddress(gBindAddr, gBindPort, gAddressFamily, gSocketType, gProtocol);
    if (res == NULL)
    {
        fprintf(stderr, "ResolveAddress failed to return any addresses!\n");
        return -1;
    }
    // For each local address returned, create a listening/receiving socket
    ptr = res;
    while (ptr)
    {
        printf("Listening address: ");
        PrintAddress(ptr->ai_addr, ptr->ai_addrlen);
        printf("\n");
        listenobj = (LISTEN_OBJ *)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(LISTEN_OBJ));
        if (listenobj == NULL)
        {
            fprintf(stderr, "Out of memory!\n");
            return -1;
        }
        listenobj->LoWaterMark = gInitialAccepts;
        InitializeCriticalSection(&listenobj->ListenCritSec);
        // Save off the address family of this socket
        listenobj->AddressFamily = ptr->ai_family;
        // create the socket
        listenobj->s = socket(ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol);
        if (listenobj->s == INVALID_SOCKET)
        {
            fprintf(stderr, "socket failed: %d\n", WSAGetLastError());
            return -1;
        }
        // Create an event to register for FD_ACCEPT events on
        listenobj->AcceptEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
        if (listenobj->AcceptEvent == NULL)
        {
            fprintf(stderr, "CreateEvent failed: %d\n", GetLastError());
            return -1;
        }
        listenobj->RepostAccept = CreateEvent(NULL, TRUE, FALSE, NULL);
        if (listenobj->RepostAccept == NULL)
        {
            fprintf(stderr, "CreateSemaphore failed: %d\n", GetLastError());
            return -1;
        }
        // Add the event to the list of waiting events
        WaitEvents[waitcount++] = listenobj->AcceptEvent;
        WaitEvents[waitcount++] = listenobj->RepostAccept;
        // Associate the socket and its SOCKET_OBJ to the completion port
        hrc = CreateIoCompletionPort((HANDLE)listenobj->s, CompletionPort, (ULONG_PTR)listenobj, 0);
        if (hrc == NULL)
        {
            fprintf(stderr, "CreateIoCompletionPort failed: %d\n", GetLastError());
            return -1;
        }
        // bind the socket to a local address and port
        rc = bind(listenobj->s, ptr->ai_addr, ptr->ai_addrlen);
        if (rc == SOCKET_ERROR)
        {
            fprintf(stderr, "bind failed: %d\n", WSAGetLastError());
            return -1;
        }
        // Need to load the Winsock extension functions from each provider
        //    -- e.g. AF_INET and AF_INET6.
        rc = WSAIoctl(
                listenobj->s,
                SIO_GET_EXTENSION_FUNCTION_POINTER,
                &guidAcceptEx,
                sizeof(guidAcceptEx),
                &listenobj->lpfnAcceptEx,
                sizeof(listenobj->lpfnAcceptEx),
                &bytes,
                NULL,
                NULL
                );
        if (rc == SOCKET_ERROR)
        {
            fprintf(stderr, "WSAIoctl: SIO_GET_EXTENSION_FUNCTION_POINTER failed: %d\n", WSAGetLastError());
            return -1;
        }
        // Load the Winsock extensions from each provider
        rc = WSAIoctl(
                listenobj->s,
                SIO_GET_EXTENSION_FUNCTION_POINTER,
                &guidGetAcceptExSockaddrs,
                sizeof(guidGetAcceptExSockaddrs),
                &listenobj->lpfnGetAcceptExSockaddrs,
                sizeof(listenobj->lpfnGetAcceptExSockaddrs),
                &bytes,
                NULL,
                NULL
                );
        if (rc == SOCKET_ERROR)
        {
            fprintf(stderr, "WSAIoctl: SIO_GET_EXTENSION_FUNCTION_POINTER failed: %d\n", WSAGetLastError());
            return -1;
        }
        // Put the socket into listening mode
        rc = listen(listenobj->s, 200);
        if (rc == SOCKET_ERROR)
        {
            fprintf(stderr, "listen failed: %d\n", WSAGetLastError());
            return -1;
        }
        // Register for FD_ACCEPT notification on listening socket
        rc = WSAEventSelect(listenobj->s, listenobj->AcceptEvent, FD_ACCEPT);
        if (rc == SOCKET_ERROR)
        {
            fprintf(stderr, "WSAEventSelect failed: %d\n", WSAGetLastError());
            return -1;
        }
        // Initiate the initial accepts for each listen socket
        for(i=0; i < gInitialAccepts ;i++)
        {
            acceptobj = GetBufferObj(gBufferSize);
            if (acceptobj == NULL)
            {
                fprintf(stderr, "Out of memory!\n");
                return -1;
            }
            acceptobj->PostAccept = listenobj->AcceptEvent;
            InsertPendingAccept(listenobj, acceptobj);
            PostAccept(listenobj, acceptobj);
        }
        // Maintain a list of the listening socket structures
        if (ListenSockets == NULL)
        {
            ListenSockets = listenobj;
        }
        else
        {
            listenobj->next = ListenSockets;
            ListenSockets   = listenobj;
        }
        endpointcount++;
        ptr = ptr->ai_next;
    }
    // free the addrinfo structure for the 'bind' address
    freeaddrinfo(res);
    gStartTime = gStartTimeLast = GetTickCount();
    interval = 0;
    while (1)
    {
        rc = WSAWaitForMultipleEvents(waitcount, WaitEvents, FALSE, 5000, FALSE);
        if (rc == WAIT_FAILED)
        {
            fprintf(stderr, "WSAWaitForMultipleEvents failed: %d\n", WSAGetLastError());
            break;
        }
        else if (rc == WAIT_TIMEOUT)
        {
            interval++;
            PrintStatistics();
            if (interval == 36)
            {
                int optval, optlen;
                // For TCP, cycle through all the outstanding AcceptEx operations
                //   to see if any of the client sockets have been connected but
                //   haven't received any data. If so, close them as they could be
                //   a denial of service attack.
                listenobj = ListenSockets;
                while (listenobj)
                {
                    EnterCriticalSection(&listenobj->ListenCritSec);
                    acceptobj = listenobj->PendingAccepts;
                    while (acceptobj)
                    {
                        optlen = sizeof(optval);
                        rc = getsockopt( acceptobj->sclient, SOL_SOCKET, SO_CONNECT_TIME, (char *)&optval, &optlen);
                        if (rc == SOCKET_ERROR)
                        {
                            fprintf(stderr, "getsockopt: SO_CONNECT_TIME failed: %d\n", WSAGetLastError());
                        }
                        else
                        {
                            // If the socket has been connected for more than 5 minutes,
                            //    close it. If closed, the AcceptEx call will fail in the completion thread.
                            if ((optval != 0xFFFFFFFF) && (optval > 300))
                            {
                                printf("closing stale handle\n");
                                closesocket(acceptobj->sclient);
                                acceptobj->sclient = INVALID_SOCKET;
                            }
                        }
                        acceptobj = acceptobj->next;
                    }
                    LeaveCriticalSection(&listenobj->ListenCritSec);
                    listenobj = listenobj->next;
                }
                interval = 0;
            }
        }
        else
        {
            int index;
            index = rc - WAIT_OBJECT_0;
            for( ; index < waitcount ; index++)
            {
                rc = WaitForSingleObject(WaitEvents[index], 0);
                if (rc == WAIT_FAILED || rc == WAIT_TIMEOUT)
                {
                    continue;
                }
                if (index < (int)sysinfo.dwNumberOfProcessors)
                {
                    // One of the completion threads exited
                    //   This is bad so just bail - a real server would want
                    //   to gracefully exit but this is just a sample ...
                    ExitProcess(-1);
                }
                else
                {
                    // An FD_ACCEPT event occurred
                    listenobj = ListenSockets;
                    while (listenobj)
                    {
                        if ((listenobj->AcceptEvent == WaitEvents[index]) ||
                                (listenobj->RepostAccept  == WaitEvents[index]))
                            break;
                        listenobj = listenobj->next;
                    }
                    if (listenobj)
                    {
                        WSANETWORKEVENTS ne;
                        int              limit=0;
                        if (listenobj->AcceptEvent == WaitEvents[index])
                        {
                            // EnumNetworkEvents to see if FD_ACCEPT was set
                            rc = WSAEnumNetworkEvents(listenobj->s, listenobj->AcceptEvent,&ne);
                            if (rc == SOCKET_ERROR)
                            {
                                fprintf(stderr, "WSAEnumNetworkEvents failed: %d\n", WSAGetLastError());
                            }
                            if ((ne.lNetworkEvents & FD_ACCEPT) == FD_ACCEPT)
                            {
                                // We got an FD_ACCEPT so post multiple accepts to cover the burst
                                limit = BURST_ACCEPT_COUNT;
                            }
                        }
                        else if (listenobj->RepostAccept == WaitEvents[index])
                        {
                            // Semaphore is signaled
                            limit = InterlockedExchange(&listenobj->RepostCount, 0);
                            ResetEvent(listenobj->RepostAccept);
                        }
                        i = 0;
                        while ( (i++ < limit) && (listenobj->PendingAcceptCount < gMaxAccepts) )
                        {
                            acceptobj = GetBufferObj(gBufferSize);
                            if (acceptobj)
                            {
                                acceptobj->PostAccept = listenobj->AcceptEvent;
                                InsertPendingAccept(listenobj, acceptobj);
                                PostAccept(listenobj, acceptobj);
                            }
                        }
                    }
                }
            }
        }
    }
    WSACleanup();
    return 0;
}
// Common routines for resolving addresses and hostnames
//
// Files:
//      resolve.cpp     - Common routines
//      resolve.h       - Header file for common routines
//
// Description:
//      This file contains common name resolution and name printing
//      routines and is used by many of the samples.
#include <winsock2.h>
#include <ws2tcpip.h>
#include <stdio.h>
#include <stdlib.h>
// Function: PrintAddress
// Description:
//    This routine takes a SOCKADDR structure and its length and prints
//    converts it to a string representation. This string is printed to the console via stdout.
int PrintAddress(SOCKADDR *sa, int salen)
{
    char    host[NI_MAXHOST], serv[NI_MAXSERV];
    int     hostlen = NI_MAXHOST, servlen = NI_MAXSERV, rc;
    rc = getnameinfo(sa, salen, host, hostlen, serv, servlen,NI_NUMERICHOST | NI_NUMERICSERV);
    if (rc != 0)
    {
        fprintf(stderr, "%s: getnameinfo() failed with error code %d\n", __FILE__, rc);
        return rc;
    }
    else
        printf("getnameinfo() is OK!\n");
    // If the port is zero then don't print it
    if (strcmp(serv, "0") != 0)
    {
        if (sa->sa_family == AF_INET)
            printf("[%s]:%s", host, serv);
        else
            printf("%s:%s", host, serv);
    }
    else
        printf("%s", host);
    return NO_ERROR;
}
// Function: FormatAddress
// Description:
//    This is similar to the PrintAddress function except that instead of
//    printing the string address to the console, it is formatted into the supplied string buffer.
int FormatAddress(SOCKADDR *sa, int salen, char *addrbuf, int addrbuflen)
{
    char    host[NI_MAXHOST], serv[NI_MAXSERV];
    int     hostlen = NI_MAXHOST, servlen = NI_MAXSERV, rc;
    rc = getnameinfo(sa, salen, host, hostlen, serv, servlen, NI_NUMERICHOST | NI_NUMERICSERV);
    if (rc != 0)
    {
        fprintf(stderr, "%s: getnameinfo() failed with error code %d\n", __FILE__, rc);
        return rc;
    }
    else
        printf("getnameinfo() is OK!\n");
    if ( (strlen(host) + strlen(serv) + 1) > (unsigned)addrbuflen)
        return WSAEFAULT;
    if (sa->sa_family == AF_INET)
        sprintf_s(addrbuf, sizeof(addrbuf), "%s:%s", host, serv);
    else if (sa->sa_family == AF_INET6)
        sprintf_s(addrbuf, sizeof(addrbuf), "[%s]:%s", host, serv);
    else
        addrbuf[0] = '\0';
    return NO_ERROR;
}
// Function: ResolveAddress
// Description:
//    This routine resolves the specified address and returns a list of addrinfo
//    structure containing SOCKADDR structures representing the resolved addresses.
//    Note that if 'addr' is non-NULL, then getaddrinfo will resolve it whether
//    it is a string literal address or a hostname.
struct addrinfo *ResolveAddress(char *addr, char *port, int af, int type, int proto)
{
    struct addrinfo hints, *res = NULL;
    int    rc;
    memset(&hints, 0, sizeof(hints));
    hints.ai_flags  = ((addr) ? 0 : AI_PASSIVE);
    hints.ai_family = af;
    hints.ai_socktype = type;
    hints.ai_protocol = proto;
    rc = getaddrinfo(addr, port, &hints, &res);
    if (rc != 0)
    {
        printf("Invalid address %s, getaddrinfo() failed with error code %d\n", addr, rc);
        return NULL;
    }
    else
        printf("getaddrinfo() should be fine!\n");
    return res;
}
