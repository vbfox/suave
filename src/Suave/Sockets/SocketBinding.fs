namespace Suave.Sockets

open System
open System.Net

/// A port is an unsigned short (uint16) structure
type Port = uint16

module IPAddress =
  let is_ipv6 (x : IPAddress) =
    x.AddressFamily = Sockets.AddressFamily.InterNetworkV6

  let to_string (x : IPAddress) =
    if is_ipv6 x then String.Concat [| "["; x.ToString(); "]" |]
    else x.ToString()

type SocketBinding =
  { ip   : IPAddress
    port : Port }
  member x.end_point =
    new IPEndPoint(x.ip, int x.port)
  override x.ToString() =
    String.Concat [ IPAddress.to_string x.ip; ":"; x.port.ToString() ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketBinding =

  let mk ip port =
    { ip   = ip
      port = port }

  let ip_ =
    (fun x -> x.ip),
    fun v x -> { x with ip = v }

  let port_ =
    (fun x -> x.port),
    fun v x -> { x with port = v }
