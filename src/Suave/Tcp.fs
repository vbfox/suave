module Suave.Tcp

// NOTE: performance tip, on mono set nursery-size with a value larger than MAX_CONCURRENT_OPS * BUFFER_SIZE
// i.e: export MONO_GC_PARAMS=nursery-size=128m
// The nursery size must be a power of two in bytes

// consider:
// echo 5 > /proc/sys/net/ipv4/tcp_fin_timeout
// echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
// custom kernel with shorter TCP_TIMEWAIT_LEN in include/net/tcp.h


open System
open System.Collections.Generic
open System.Threading
open System.Net
open System.Net.Sockets

open Suave.Logging
open Suave.Sockets

/// The max backlog of number of requests
[<Literal>]
let MaxBacklog = Int32.MaxValue

type StartedData =
  { start_called : DateTimeOffset
    socket_bound : DateTimeOffset option
    binding      : SocketBinding }
with
  override x.ToString() =
    sprintf "%A" x

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StartedData =

  let to_text data =
    sprintf "%.3f ms with binding %O:%d"
      ((data.socket_bound |> Option.fold (fun _ t -> t) data.start_called) - data.start_called).TotalMilliseconds
      data.binding.ip data.binding.port

  let start binding =
    { start_called = Globals.now ()
      socket_bound = None
      binding      = binding }

  let started data =
    { data with socket_bound = Some (Globals.now ()) }

module internal Impl =

  /// Disconnect a socket for reuse
  let close_socket (s : Socket) =
    try
      if s <> null then
        if s.Connected || s.IsBound then 
          s.Disconnect(true)
    with _ -> ()

  /// Shoots down a socket for good
  let shutdown_socket (s : Socket) =
    try
      if s <> null then
        try
          s.Shutdown(SocketShutdown.Both)
        with _ -> ()
        s.Close ()
        s.Dispose ()
    with _ -> ()

  /// Stop the TCP listener server
  let stop_tcp (logger : Logger) reason (socket : Socket) =
    try
      Log.verbosef logger "Tcp.stop_tcp" (fun fmt -> fmt "stopping tcp server, reason: '%s'" reason)
      socket.Close()
      "stopped tcp server" |> Log.verbose logger "Tcp.stop_tcp"
    with ex ->
      "failure stopping tcp server" |> Log.verbosee logger "Tcp.stop_tcp" ex

  type SAEAP = SocketAsyncEventArgsPool
  type SAEA = SocketAsyncEventArgs

  /// An accept, read, write args pool and a buffer manager
  type Pools = SAEAP * SAEAP * SAEAP * BufferManager

  let create_pools logger max_ops buffer_size : Pools =

    let accept_pool = new SocketAsyncEventArgsPool()
    let read_pool   = new SocketAsyncEventArgsPool()
    let write_pool  = new SocketAsyncEventArgsPool()

    let buffer_manager = new BufferManager(buffer_size * (max_ops + 1), buffer_size, logger)
    buffer_manager.Init()

    for x = 0 to max_ops - 1 do
      //Pre-allocate a set of reusable SocketAsyncEventArgs
      let read_evt_arg, ut = new SocketAsyncEventArgs(),
                             new AsyncUserToken()
      read_evt_arg.UserToken <- ut
      read_evt_arg.add_Completed(fun a b -> ut.Continuation b)

      read_pool.Push read_evt_arg

      let write_evt_arg, ut = new SocketAsyncEventArgs(),
                              new AsyncUserToken()
      write_evt_arg.UserToken <- ut
      write_evt_arg.add_Completed(fun a b -> ut.Continuation b)

      write_pool.Push write_evt_arg

      let accept_evt_arg, ut = new SocketAsyncEventArgs(),
                               new AsyncUserToken()
      accept_evt_arg.UserToken <- ut
      accept_evt_arg.add_Completed(fun a b -> ut.Continuation b)

      accept_pool.Push accept_evt_arg

    accept_pool, read_pool, write_pool, buffer_manager

  let try_repeatedly f =
    let s ms = Thread.Sleep (ms : int)
    let rec run = function
      | 0us | 1us -> f ()
      | n -> try f () with e -> s 10; run (n - 1us)
    run 3us

  let job_runner logger
                 ((accept_pool, read_pool, write_pool, buffer_manager) : Pools)
                 (serve_client : TcpWorker<unit>) =

    fun (accept_args : SocketAsyncEventArgs) -> async {
      let intern  = Log.debug logger "Suave.Tcp.tcp_ip_server.job"
      let socket = accept_args.AcceptSocket
      let ip_address = (socket.RemoteEndPoint :?> IPEndPoint).Address
      Interlocked.Increment Globals.number_of_clients |> ignore

      Log.verbosef logger "Suave.Tcp.tcp_ip_server.job" (fun fmt -> fmt "%O connected, total: %d clients" ip_address !Globals.number_of_clients)

      try
        let read_args, write_args = read_pool.Pop(), write_pool.Pop()
        let connection =
          { ipaddr    = ip_address
            transport =
              { socket     = socket
                read_args  = read_args
                write_args = write_args}
            buffer_manager = buffer_manager
            line_buffer  = buffer_manager.PopBuffer "Suave.Tcp.tcp_ip_server.job"
            segments     = []
          }
        use! oo = Async.OnCancel (fun () -> intern "disconnected client (async cancel)"
                                            shutdown_socket socket)

        let! _ = serve_client connection
        shutdown_socket socket
        accept_args.AcceptSocket <- null
        accept_pool.Push accept_args
        read_pool.Push read_args
        write_pool.Push write_args
        buffer_manager.FreeBuffer(connection.line_buffer, "Suave.Tcp.tcp_ip_server.job")
        Interlocked.Decrement(Globals.number_of_clients) |> ignore
        Log.verbosef logger "Suave.Tcp.tcp_ip_server.job" (fun fmt -> fmt "%O disconnected, total: %d clients" ip_address !Globals.number_of_clients)
      with
      | :? System.IO.EndOfStreamException ->
        intern "disconnected client (end of stream)"
      | ex -> "tcp request processing failed" |> Log.verbosee logger "Suave.Tcp.tcp_ip_server.job" ex
    }

  let accept_loop logger 
                  (accepting : AsyncResultCell<_>)
                  binding
                  listen_socket
                  (accept_pool : SAEAP)
                  (run_job : SAEA -> Async<_>)
                  =
    async {
      let start_data = StartedData.start binding

      try
        use! dd = Async.OnCancel(fun () -> stop_tcp logger "tcp_ip_server async cancelled" listen_socket)
        let! token = Async.CancellationToken

        let start_data = StartedData.started start_data
        accepting.Complete start_data

        logger.Log
          { path          = "Suave.Tcp.tcp_ip_server"
            trace         = TraceHeader.empty
            message       = sprintf "listener started in %O%s" start_data (if token.IsCancellationRequested then ", cancellation requested" else "")
            level         = LogLevel.Info
            ``exception`` = None
            data          = Map.empty
            tags          = Set.empty
            timestamp     = Globals.now() }

        while not (token.IsCancellationRequested) do
          try
            let accept_args = accept_pool.Pop()
            let! res = accept listen_socket accept_args
            Async.Start (run_job accept_args, token)
          with ex -> "failed to accept a client" |> Log.verbosee logger "Suave.Tcp.tcp_ip_server" ex
        return ()
      with ex ->
        "tcp server failed" |> Log.verbosee logger "Suave.Tcp.tcp_ip_server" ex
        return ()
    }

open Impl

/// Start a new TCP server with a specific IP, Port and with a serve_client worker
/// returning an async workflow whose result can be awaited (for when the tcp server has started
/// listening to its address/port combination), and an asynchronous workflow that
/// yields when the full server is cancelled. If the 'has started listening' workflow
/// returns None, then the start timeout expired.
let tcp_ip_server (buffer_size        : int,
                   max_concurrent_ops : int)
                  (logger             : Logger)
                  (serve_client       : TcpWorker<unit>)
                  (binding            : SocketBinding) =

  let listen_socket = new Socket(binding.end_point.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
  let accepting = new AsyncResultCell<StartedData>()

  let (accept_pool, read_pool, write_pool, buffer_manager) as pools =
    create_pools logger max_concurrent_ops buffer_size

  try_repeatedly (fun () -> listen_socket.Bind binding.end_point)
  listen_socket.Listen MaxBacklog

  accepting.AwaitResult (), // accepting connections
  accept_loop logger accepting binding
              listen_socket accept_pool
              (job_runner logger pools serve_client)