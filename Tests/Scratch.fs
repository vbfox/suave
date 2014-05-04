module Suave.Scratch

#if INTERACTIVE
#r "System.Xml"
#r "System.Xml.Linq"
#r "../packages/NodaTime.1.2.0/net40/NodaTime.dll"
#endif

open System
open System.IO

open NodaTime

[<RequireQualifiedAccess>]
module UTF8 =
  open System
  open System.Text

  let inline to_string (b : byte []) (index : int) (count : int) =
    Encoding.UTF8.GetString(b, index, count)

  let inline to_string' (b : byte []) =
    Encoding.UTF8.GetString b

  /// Get the UTF-8 bytes for the string
  let inline bytes (s : string) =
    Encoding.UTF8.GetBytes s

  /// Encode the string as UTF8 encoded in Base64.
  let inline base64_encode (s : string) =
    let bytes = Encoding.UTF8.GetBytes s
    Convert.ToBase64String bytes

  let inline base64_decode s =
    let bytes = Convert.FromBase64String s
    Encoding.UTF8.GetString bytes

[<RequireQualifiedAccess>]
module ASCII =
  open System
  open System.Text

  /// Get the ASCII bytes for the string
  let inline bytes (s : string) =
    Encoding.ASCII.GetBytes s

  /// Convert the byte array of ASCII-encoded chars to a string, starting at 'index' for 'count' characters
  /// (each character is necessarily one byte)
  let inline to_string (buff : byte[]) (index : int) (count : int) =
    Encoding.ASCII.GetString(buff, index, count)

  let inline to_string' (b : byte []) =
    Encoding.ASCII.GetString b

  /// Encode the string as ASCII encoded in Base64.
  let inline base64_encode (s : string) =
    let bytes = Encoding.ASCII.GetBytes s
    Convert.ToBase64String bytes

  /// Decode the string containing Base64-encoded ASCII string data to a string
  let inline base64_decode (s : string) =
    let bytes = Convert.FromBase64String s
    Encoding.ASCII.GetString bytes

module Option =
  let orDefault value opt =
    opt |> Option.fold (fun s t -> t) value

module Abstractions =

  let internal noexn f =
    try f () with _ -> ()

  let internal memoize f =
    let cache = ref Map.empty
    fun x ->
      match (!cache).TryFind(x) with
      | Some res -> res
      | None ->
        let res = f x
        cache := (!cache).Add(x,res)
        res

  module DateTime =
    open System.Globalization

    /// the format of all HTTP headers
    type RFC1132 = Instant -> string

    /// the recommended format for input/output with APIs
    type ISO8601 = Instant -> string
    type RFC3339 = ISO8601

    /// an abstraction for the clock
    type Clock =
      abstract Now : unit -> Instant

    let system_clock =
      { new Clock with
          member x.Now () = SystemClock.Instance.Now }

    let iso8601 : ISO8601 =
      // http://nodatime.org/1.2.x/userguide/instant-patterns.html
      fun i -> i.ToString("g", CultureInfo.InvariantCulture)

    let rfc1132 : RFC1132 =
      // http://stackoverflow.com/a/6668866
      fun i -> i.ToDateTimeUtc().ToString("R", CultureInfo.InvariantCulture)

  [<AutoOpen>]
  module ClockExtensions =
    open DateTime

    type Clock with
      member x.RFC1132 () =
        rfc1132 <| x.Now()
      member x.ISO8601 () =
        iso8601 <| x.Now ()

  module Problems =
    type HelpLink = Uri

    type RecommendedAction =
      | DoNothing
      | RestartServer
      | NotifySysadmin

    type Known =
      | ClientDisconnected
      | ClientTooSlowReader // resource starvation attack, or just slow client
      | ClientTooSlowWriter // resource starvation attack, or just slow client
      // e.g. service is missing environmental resources, incorrectly provisioned
      | ServiceUnavailable

    type Problem =
      { ``exception`` : exn option
        desc          : string
        known         : Known option
        help          : HelpLink option * RecommendedAction }

    /// request is misformed
    exception InvalidHttpRequest of Problem
    /// request does not start out as proper HTTP request
    exception NoHttpRequest of Problem

  module Fs =
    type FilePath = string

    type FileSnapshot =
      { created : Instant
        updated : Instant }

    type FileSystem =
      abstract Open : FilePath -> FileStream * FileSnapshot

  module Logging =

    type LoggerPath = string

    type TraceHeader =
      { trace_id      : uint64
        req_id        : uint64
        req_parent_id : uint64 option }

    type LogLevel =
      | Verbose
      | Debug
      | Info
      | Warn
      | Error
      | Fatal

    type LogLine =
      { trace         : TraceHeader option
        level         : LogLevel
        path          : LoggerPath
        message       : string
        ``exception`` : exn option
        timestamp     : Instant
        tags          : string list }

    type Logger =
      abstract member Log : LogLevel -> (unit -> LogLine) -> unit

    module Log =
      let verbose (logger : Logger) (f_line : unit -> LogLine) =
        logger.Log Verbose f_line
      let debug (logger : Logger) (f_line : unit -> LogLine) =
        logger.Log Debug f_line
      let info (logger : Logger) (f_line : unit -> LogLine) =
        logger.Log Info f_line

  module StreamTransducers =

    


  module ServerEvents =
    open Logging
    open DateTime
    open System.Net

    let [<Literal>] server_event = "ss:evt"

    let server_spawned (ep : IPEndPoint) (clock : Clock) () =
      { trace         = None
        level         = Info
        path          = "suave"
        message       = sprintf "listening to %O:%d" ep.Address ep.Port
        ``exception`` = None
        timestamp     = clock.Now()
        tags          = [ server_event ] }

    let client_connected (ep : EndPoint) (binding : IPEndPoint) (clock : Clock) () =
      { trace         = None
        level         = Debug
        path          = "suave.handle_peer"
        message       = sprintf "client connected [%O]" ep
        ``exception`` = None
        timestamp     = clock.Now()
        tags          = [ server_event ] }

    let client_disconnected (ep : EndPoint) (binding : IPEndPoint) (clock : Clock) () =
      { trace         = None
        level         = Debug
        path          = "suave.handle_peer"
        message       = sprintf "client disconnected [%O]" ep
        ``exception`` = None
        timestamp     = clock.Now()
        tags          = [ server_event ] }

  module Persistence =

    // TODO: revisit our own Cache abstractions, incorporate?
    type ReadStore =
      //abstract ReadStream : string (* key *) -> Async<Stream option>
      abstract ReadBytes  : string (* key *) -> Async<byte[] option>

    // TODO: revisit our own Cache abstractions, incorporate?
    type WriteStore =
      //abstract WriteStream : string (* key *) -> Stream -> Async<unit>
      abstract WriteBytes : string (* key *) -> Stream -> Async<unit>

    type KVStore =
      inherit ReadStore
      inherit WriteStore

    // TODO: provide sqlite provider for read/write-stores above
    // TODO: provide riak provider for read/write-stores above for eventually
    //       consistent data -- and provide statebox equivalent

module Http =
  open Abstractions.Logging

  type HttpCode =
    | HTTP_100 | HTTP_101
    | HTTP_200 | HTTP_201 | HTTP_202 | HTTP_203 | HTTP_204 | HTTP_205 | HTTP_206
    | HTTP_300 | HTTP_301 | HTTP_302 | HTTP_303 | HTTP_304 | HTTP_305 | HTTP_307
    | HTTP_400 | HTTP_401 | HTTP_402 | HTTP_403 | HTTP_404 | HTTP_405 | HTTP_406
    | HTTP_407 | HTTP_408 | HTTP_409 | HTTP_410 | HTTP_411 | HTTP_412 | HTTP_413
    | HTTP_422 | HTTP_428 | HTTP_429 | HTTP_414 | HTTP_415 | HTTP_416 | HTTP_417
    | HTTP_500 | HTTP_501 | HTTP_502 | HTTP_503 | HTTP_504 | HTTP_505
    static member TryParse (code : int) =
      // TODO: replace with match code with | 100 -> HTTP_100 | ... when API is more set
      let cases = Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<HttpCode>)
      let map_cases =
        cases
        |> Array.map (fun case -> case.Name, Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(case, [||]) :?> HttpCode)
        |> Map.ofArray
      map_cases |> Map.tryFind ("HTTP_" + code.ToString())

  let http_code = function
    | HTTP_100 -> 100 | HTTP_101 -> 101 | HTTP_200 -> 200 | HTTP_201 -> 201
    | HTTP_202 -> 202 | HTTP_203 -> 203 | HTTP_204 -> 204 | HTTP_205 -> 205
    | HTTP_206 -> 206 | HTTP_300 -> 300 | HTTP_301 -> 301 | HTTP_302 -> 302
    | HTTP_303 -> 303 | HTTP_304 -> 304 | HTTP_305 -> 305 | HTTP_307 -> 307
    | HTTP_400 -> 400 | HTTP_401 -> 401 | HTTP_402 -> 402 | HTTP_403 -> 403
    | HTTP_404 -> 404 | HTTP_405 -> 405 | HTTP_406 -> 406 | HTTP_407 -> 407
    | HTTP_408 -> 408 | HTTP_409 -> 409 | HTTP_410 -> 410 | HTTP_411 -> 411
    | HTTP_412 -> 412 | HTTP_413 -> 413 | HTTP_414 -> 414 | HTTP_415 -> 415
    | HTTP_416 -> 416 | HTTP_417 -> 417 | HTTP_422 -> 422 | HTTP_428 -> 428
    | HTTP_429 -> 429 | HTTP_500 -> 500 | HTTP_501 -> 501 | HTTP_502 -> 502
    | HTTP_503 -> 503 | HTTP_504 -> 504 | HTTP_505 -> 505

  let http_reason = function
    | HTTP_100 -> "Continue"
    | HTTP_101 -> "Switching Protocols"
    | HTTP_200 -> "OK"
    | HTTP_201 -> "Created"
    | HTTP_202 -> "Accepted"
    | HTTP_203 -> "Non-Authoritative Information"
    | HTTP_204 -> "No Content"
    | HTTP_205 -> "Reset Content"
    | HTTP_206 -> "Partial Content"
    | HTTP_300 -> "Multiple Choices"
    | HTTP_301 -> "Moved Permanently"
    | HTTP_302 -> "Found"
    | HTTP_303 -> "See Other"
    | HTTP_304 -> "Not Modified"
    | HTTP_305 -> "Use Proxy"
    | HTTP_307 -> "Temporary Redirect"
    | HTTP_400 -> "Bad Request"
    | HTTP_401 -> "Unauthorized"
    | HTTP_402 -> "Payment Required"
    | HTTP_403 -> "Forbidden"
    | HTTP_404 -> "Not Found"
    | HTTP_405 -> "Method Not Allowed"
    | HTTP_406 -> "Not Acceptable"
    | HTTP_407 -> "Proxy Authentication Required"
    | HTTP_408 -> "Request Timeout"
    | HTTP_409 -> "Conflict"
    | HTTP_410 -> "Gone"
    | HTTP_411 -> "Length Required"
    | HTTP_412 -> "Precondition Failed"
    | HTTP_413 -> "Request Entity Too Large"
    | HTTP_414 -> "Request-URI Too Long"
    | HTTP_415 -> "Unsupported Media Type"
    | HTTP_416 -> "Requested Range Not Satisfiable"
    | HTTP_417 -> "Expectation Failed"
    | HTTP_422 -> "Unprocessable Entity"
    | HTTP_428 -> "Precondition Required"
    | HTTP_429 -> "Too Many Requests"
    | HTTP_500 -> "Internal Server Error"
    | HTTP_501 -> "Not Implemented"
    | HTTP_502 -> "Bad Gateway"
    | HTTP_503 -> "Service Unavailable"
    | HTTP_504 -> "Gateway Timeout"
    | HTTP_505 -> "HTTP Version Not Supported"

  let http_message = function
    | HTTP_100 -> "Request received, please continue"
    | HTTP_101 -> "Switching to new protocol; obey Upgrade header"
    | HTTP_200 -> "Request fulfilled, document follows"
    | HTTP_201 -> "Document created, URL follows"
    | HTTP_202 -> "Request accepted, processing continues off-line"
    | HTTP_203 -> "Request fulfilled from cache"
    | HTTP_204 -> "Request fulfilled, nothing follows"
    | HTTP_205 -> "Clear input form for further input."
    | HTTP_206 -> "Partial content follows."
    | HTTP_300 -> "Object has several resources -- see URI list"
    | HTTP_301 -> "Object moved permanently -- see URI list"
    | HTTP_302 -> "Object moved temporarily -- see URI list"
    | HTTP_303 -> "Object moved -- see Method and URL list"
    | HTTP_304 -> "Document has not changed since given time"
    | HTTP_305 -> "You must use proxy specified in Location to access this resource."
    | HTTP_307 -> "Object moved temporarily -- see URI list"
    | HTTP_400 -> "Bad request syntax or unsupported method"
    | HTTP_401 -> "No permission -- see authorization schemes"
    | HTTP_402 -> "No payment -- see charging schemes"
    | HTTP_403 -> "Request forbidden -- authorization will not help"
    | HTTP_404 -> "Nothing matches the given URI"
    | HTTP_405 -> "Specified method is invalid for this resource."
    | HTTP_406 -> "URI not available in preferred format."
    | HTTP_407 -> "You must authenticate with this proxy before proceeding."
    | HTTP_408 -> "Request timed out; try again later."
    | HTTP_409 -> "Request conflict."
    | HTTP_410 -> "URI no longer exists and has been permanently removed."
    | HTTP_411 -> "Client must specify Content-Length."
    | HTTP_412 -> "Precondition in headers is false."
    | HTTP_413 -> "Entity is too large."
    | HTTP_414 -> "URI is too long."
    | HTTP_415 -> "Entity body in unsupported format."
    | HTTP_416 -> "Cannot satisfy request range."
    | HTTP_417 -> "Expect condition could not be satisfied."
    | HTTP_422 -> "The entity sent to the server was invalid."
    | HTTP_428 -> "You should verify the server accepts the request before sending it."
    | HTTP_429 -> "Request rate too high, chill out please."
    | HTTP_500 -> "Server got itself in trouble"
    | HTTP_501 -> "Server does not support this operation"
    | HTTP_502 -> "Invalid responses from another server/proxy."
    | HTTP_503 -> "The server cannot process the request due to a high load"
    | HTTP_504 -> "The gateway server did not receive a timely response"
    | HTTP_505 -> "Cannot fulfill request."

  type HttpCode with
    member x.Describe () =
      sprintf "%d %s: %s" (http_code x) (http_reason x) (http_message x)

  type HttpProtocol =
    | HTTP
    | HTTPS

  // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
  // http://tools.ietf.org/html/draft-ietf-httpbis-http2-12
  type HttpVersion =
    | V11
    | V20

  type HttpMethod =
    | GET
    | POST
    | DELETE
    | PUT
    | HEAD
    | CONNECT
    | PATCH
    | TRACE
    | OPTIONS

  module HeaderData =
    // Accept
    type MediaRange = string // currently an alias to MediaType too
    type MediaType  = string
    type AcceptParams = string
    type Token        = string
    type QuotedString = string
    type TokenOrQuotedString = string

    /// seconds
    [<Measure>] type s

    // Accept-Ranges
    type AcceptRanges =
      | AR_Bytes
      | AR_None

    // Cache-Control
    type FieldName = string

    type CacheReqDir =
      | CReq_NoCache
      | CReq_NoStore
      | CReq_MaxAge of int<s>
      | CReq_MaxStale of int<s> option
      | CReq_MinFresh of int<s>
      | CReq_NoTransform
      | CReq_OnlyIfCached
      | CReq_CacheExt of string

    type CacheRespDir =
      | CResp_Public
      | CResp_Private of FieldName option
      | CResp_NoCache of int<s> option
      | CResp_NoStore
      | CResp_NoTransform
      | CResp_MustRevalidate
      | CResp_ProxyRevalidate
      | CResp_MaxAge of int<s>
      | CResp_SMaxAge of int<s>
      | CResp_CacheExt of string

    // Connection: request only
    type ConnectionToken =
      | Conn_Close
      | Conn_KeepAlive
      | Conn_Upgrade of string // with list of supported protocols,
                               // tools.ietf.org/html/draft-ietf-httpbis-http2-12
                               // also see Upgrade header

    // zero index based, inclusive, e.g. bytes 0-499/1234 for first 500 bytes
    type ContentRangeSpec =
      { first : uint64
        last  : uint64
        instance_len : uint64 }
      override x.ToString() =
        sprintf "bytes %d-%d/%d" x.first x.last x.instance_len

    // Expect 14.20
    type Expectation =
      | Exp_Cont100
      // TODO: can be represented better
      | Exp_Ext of Token * (TokenOrQuotedString * string option) option // string=params
      override x.ToString() =
        match x with // TODO: unit tests, proper printing, proper representation
        | Exp_Cont100 -> "100-continue"
        | Exp_Ext (token, Some (eq_value, m_pars)) ->
          sprintf "%s=%s%s" token eq_value
            (m_pars |> Option.fold (fun _ pars -> sprintf ";%s" pars) "")
        | Exp_Ext (token, None) ->
          token

    // Pragma 14.32
    type PragmaSpec =
      | Prag_NoCache
      | Prag_Ext of Token * TokenOrQuotedString option

    // Retry-After 14.37
    type RetryAfterSpec =
      | RA_Date of Instant
      | RA_Seconds of int<s>

  open HeaderData

  /// http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
  type RequestHeaders =
    | Accept of MediaRange * AcceptParams option // end-to-end
    | ``Accept-Charset`` of string // end-to-end
    | ``Accept-Encoding`` of string // end-to-end
    | ``Accept-Language`` of string // end-to-end
    | ``Accept-Ranges`` of AcceptRanges // end-to-end
    | Age of int<s>
    | Allow of HttpMethod list
    | Authorization of string
    | ``Cache-Control`` of CacheReqDir // end-to-end
    | Connection of ConnectionToken // point-to-point (segment)
    | Expect of Expectation // server may respond 417 Expectation Failed, or 100 Continue
    | From of string // email
    | Host of string // required field, otherwise respond 400
    | ``If-Match`` of string
    | ``If-Modified-Since`` of string
    | ``If-None-Match`` of string
    | ``If-Range`` of string
    | ``If-Unmodified-Since`` of Instant // ignore if invalid
    | ``Max-Forwards`` of uint16
    | Pragma of PragmaSpec // even thuo a general header, only applicative to req
    | Range of string // TODO: byte-range-specifier is more complex than ContentRangeSpec
    | Referer (* sic *) of Uri // absolute or relative, 14.36
    | TE of string // 14.39 TODO: revisit when doing chunked transfer encoding
                   // and deflate/gzip/snappy transfer encodings
    | Trailer of string // not allowed: Trailer, Transfer-Encoding, Content-Length
    | ``Transfer-Encoding`` of string // 14.41, also 3.6
    | Warning of string

  /// http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
  type Base64 = string

  // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
  // http://tools.ietf.org/html/draft-ietf-httpbis-http2-12
  type ResponseHeaders =
    | ``Cache-Control`` of CacheRespDir
    | ``Content-Encoding`` of string
    | ``Content-Language`` of string list // comma sep
    | ``Content-Length`` of uint64 // transfer len of msg body
    | ``Content-Location`` of Uri // rel or abs
    | ``Content-MD5`` of Base64
    | ``Content-Range`` of ContentRangeSpec // with 206 Partial Content
    | ``Content-Type`` of MediaType
    | Date of Instant // date always in RFC 1123 format
    | ETag of QuotedString
    | ``HTTP2-Settings`` of Base64
    | ``Last-Modified`` of Instant // date always in RFC 1123 format
    | Location of Uri // only absolute Uris
    | ``Proxy-Authenticate``  of string // challenge, w/ 407 Proxy Authentication Required
    | ``Proxy-Authorization`` of string // creds
    | ``Retry-After`` of RetryAfterSpec // useful with 503 Service Unavailable
    | Trailer of string // not allowed: Trailer, Transfer-Encoding, Content-Length
    | ``Transfer-Encoding`` of string // 14.41, also 3.6
    | Server of string
    | Warning of string
    | ``WWW-Authenticate`` of string // challenge, with 401 Unauthorized status

  module Conneg =
    ()

  type HttpCookie =
    { name      : string
      value     : string
      expires   : DateTimeOffset option
      path      : string option
      domain    : string option
      secure    : bool
      http_only : bool
      version   : string option }

  type UriCreds = string * string

  type Path = string

  type Uri =
    { scheme       : string // https
      credentials  : UriCreds option // username:password (@example.com)
      host         : string // api.example.com OR [34::3566::1] OR 127.0.0.1
      port         : uint16 // 8080
      path         : Path // /api/stuff
      query        : Map<string, string option> // ?a=b&c=&d
      frag         : string option } // #frag

  type HttpHeaders = Map<string, string>

  type RawReq =
    { ``method`` : HttpMethod
      path       : Path
      http_ver   : HttpVersion
      headers    : HttpHeaders }

  type Req =
    { raw    : RawReq
      logger : Logger
      trace  : TraceHeader }

  type StatusCode = uint16

  type HttpReason = string

  type RespBody =
    | RawBytes of byte []
    | Streamed of Stream

  type Resp =
    { resp_line : HttpVersion * StatusCode * HttpReason
      headers   : Map<string, string> }

  /// The UserContext is a map of items passed on from previous applicatives,
  /// writers or calls.
  type UserContext = Map<string, obj>

  /// The HttpContext is a collection of the input and output
  type HttpContext = Req * Resp * UserContext

module App =
  open Http
  open Abstractions.Problems

  /// A writer has no task other than modifying the state going forward.
  type Writer = HttpContext -> HttpContext

  /// <summary><para>
  /// A web part is a thing that executes on a Req, asynchronously, maybe executing
  /// on the request.
  /// <para></para>
  /// You can do (:Applicative) >>= (:WebPart), because Applicative returns HttpContext option
  /// and WebPart takes HttpContext and is called if the return value from the applicative
  /// is Some value.
  /// </para></summary>
  type WebPart = HttpContext -> Async<unit> option

  /// An error handler takes the problem and returns an asynchronous workflow
  /// for the handling of the error.
  type ErrorHandler = Problem -> WebPart

  type Applicative = HttpContext -> HttpContext option

module SocketApi =
  open System.Net
  open System.Net.Sockets
  open System.Collections.Generic

  let default_flags = new SocketFlags()

  let private toIList<'T> (data : 'T array) =
    let segment = new ArraySegment<'T>(data)
    let data = new List<ArraySegment<'T>>() :> IList<ArraySegment<'T>>
    data.Add segment
    data

  module Options =
    let private socket_level = SocketOptionLevel.Socket

    let linger (s : Socket) (value : bool) =
      s.SetSocketOption(socket_level, SocketOptionName.Linger, value)

    let reuse_address (s : Socket) (value : bool) =
      s.SetSocketOption(socket_level, SocketOptionName.ReuseAddress, value)

    let send_timeout (s : Socket) (value : Duration) =
      s.SetSocketOption(socket_level,
                        SocketOptionName.SendTimeout,
                        value.ToTimeSpan().TotalMilliseconds |> int)

    let receive_timeout (s : Socket) (value : Duration) =
      s.SetSocketOption(socket_level,
                        SocketOptionName.ReceiveTimeout,
                        value.ToTimeSpan().TotalMilliseconds |> int)

  let bind (s : Socket) ep =
    s.Bind ep

  /// make and bind the socket
  let mk_socket (ep : IPEndPoint) =
    let s = new Socket(ep.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
    ep |> bind s
    s

  let listen (s : Socket) (backlog : uint16) =
    s.Listen (int backlog)

  let accept (s : Socket) (recv_size : uint16) =
    Async.FromBeginEnd(
      recv_size,
      (fun (recv_size, callback, state) -> s.BeginAccept(int recv_size, callback, state)),
      s.EndAccept)

  let connect (s : Socket) (ip : IPAddress, port : uint16) =
    Async.FromBeginEnd(
      (fun (cb, st) -> s.BeginConnect(ip, int port, cb, st)),
      s.EndConnect)

  let close (s : Socket) timeout =
    s.Close timeout

  let send (s : Socket) (flags : SocketFlags) (data : byte []) =
    Async.FromBeginEnd(
      toIList data, flags,
      (fun (data, flags, callback, state) -> s.BeginSend(data, flags, callback, state)),
      s.EndSend)

  let recv (s : Socket) (flags : SocketFlags) (buffer : byte []) =
    Async.FromBeginEnd(
      toIList buffer, flags,
      (fun (data, flags, callback, state) -> s.BeginReceive(data, flags, callback, state)),
      s.EndReceive)

module HttpStreamReader =
  open Abstractions
  open Abstractions.StreamTransducers
  open Abstractions.Problems
  open Http

  type Header = string * string

  type StreamParseState =
    { position  : int
      available : int }

  let mk_reader (io_stream : Stream) : Plan<StreamParseState, Header, Choice<Req, Problem>> =
    //{ todo = "yup" }
    io_stream

module internal Implementation =
  open Http
  open HttpStreamReader
  open Abstractions
  open Abstractions.Problems
  open Abstractions.StreamTransducers

  let respond (code : HttpCode) data =
    async { return () } |> Some

  /// returns true if connection keep alive
  let serve_request (machine : Plan<StreamParseState, Header, Req>) =
    try
      // TODO: implement the protocol of parsing the headers and acting
      // on them
      machine.Run()
    with
    | NoHttpRequest problem ->
      machine.Dispose()
    | InvalidHttpRequest problem ->
      respond HTTP_400 (http_reason HTTP_400)

module HttpServer =
  open Abstractions
  open Abstractions.Logging
  open Abstractions.DateTime
  open SocketApi
  open Machines
  open Implementation

  open System.Threading
  open System.Net
  open System.Net.Sockets

  module TLS =
    open Abstractions.Persistence
    open Abstractions.DateTime

    type ProtocolVersion =
      | SSL_3p0
      | TLS_1p0
      | TLS_1p1
      | TLS_1p2

    type TLSOptions =
      { minVer       : ProtocolVersion
        maxVer       : ProtocolVersion
        session_db   : KVStore }
      // TODO: TLS options of miTLS

  open TLS

  type AdvancedConfig =
    { socket_backlog      : uint16
      receive_buffer_size : uint16
      ct                  : CancellationToken
      wall_clock          : Clock
      random              : Random
      crypt_random        : System.Security.Cryptography.RandomNumberGenerator
      mk_tls_stream       : Stream -> TLSOptions -> Stream }

  type Config =
    { binding   : IPEndPoint * TLS.TLSOptions option
      mk_logger : LoggerPath -> Logger
      advanced  : AdvancedConfig }

  type private ServerState =
    { socket : Socket }

  let rec private handle_peer ({ binding = b } as config) (peer : Socket) = async {
    let ep     = fst config.binding
    let clock  = config.advanced.wall_clock
    let logger = config.mk_logger "suave.handle_peer"
    ServerEvents.client_connected peer.RemoteEndPoint ep clock |> Log.debug logger
    try
      use ns = new NetworkStream(peer)
      let io_stream : Stream =
        match config.binding |> snd with
        | Some tls_options -> config.advanced.mk_tls_stream (upcast ns) tls_options
        | None             -> upcast ns
      let! cont = serve_request (mk_reader io_stream)
      if cont then return! handle_peer config peer
      else return ()
    finally
      ServerEvents.client_disconnected peer.RemoteEndPoint ep clock |> Log.debug logger
      noexn (fun () -> close peer 0)
    }

  let private server config =

    let rec start () = async {
      let ep, tls = config.binding
      let socket = mk_socket ep
      false |> Options.linger socket
      true |> Options.reuse_address socket
      do listen socket config.advanced.socket_backlog
      return! running { socket = socket }
      }

    and running state = async {
      if config.advanced.ct.IsCancellationRequested then
        return! shutdown state
      else
        let! peer = accept state.socket config.advanced.receive_buffer_size
        Async.Start(handle_peer config peer, config.advanced.ct)
        return! running state
      }

    and shutdown state = async {
      state.socket.Shutdown(SocketShutdown.Both)
      state.socket.Close(0)
      return ()
      }

    start ()

  let run config = async {
    let config = { config with mk_logger = memoize config.mk_logger }
    return server config, async { return () }
    }

module HttpClient =
  open System
  open System.Net
  open System.Net.Sockets
  open System.Text
  open System.Text.RegularExpressions

  open SocketApi

  type ClientInstance =
    { socket : Socket }

  /// creates a connected socket
  let with_client (target : IPAddress, port : uint16) f_client = async {
    use socket = new Socket(target.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
    do! (target, port) |> connect socket
    return! f_client { socket = socket }
    }

  let as_raw_request clr_string =
    let crlf = "\r\n" |> ASCII.bytes
    use sr = new StringReader(clr_string)
    use ms = new MemoryStream()
    let rec read_it () =
      match sr.ReadLine() with
      | null ->
        ms.ToArray()
      | line ->
        let bs = line |> ASCII.bytes
        ms.Write(bs, 0, bs.Length)
        ms.Write(crlf, 0, crlf.Length)
        read_it ()
    read_it ()

  /// send the raw data to the socket
  let netcat (client : ClientInstance) data =
    data |> send client.socket default_flags

  type HttpResponse =
    { headers : Map<string, string> }

  let read_response (c : ClientInstance) =
    let body_started (line : string) =
      line = null || line = ""

    let parse_header (line : string) =
      let gs = Regex.Match(line, "^(?<key>\w+): (?<val>.*)$").Groups
      gs.["key"].Value, gs.["val"].Value

    use ns = new NetworkStream(c.socket)
    use sr = new StreamReader(ns)

    sr.ReadLine()
    |> Seq.unfold (fun s -> if not(body_started s) then Some(s, sr.ReadLine()) else None)
    |> Seq.map parse_header
    |> Map.ofSeq
    |> fun m -> { headers = m }

module Tests =
  open HttpClient
  open SocketApi
  open System.Net

  open Fuchu

  [<Tests>]
  let functional_tests =
    testList "connecting and getting response" [
      testCase "GET / -> 200 OK  and 'Hello World'" (fun _ ->
        let ip, port = IPAddress.Parse("::1"), 8080us
        with_client (ip, port)
          (fun client -> async {
            let str = "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n"
            do! str
                |> as_raw_request
                |> netcat client
                |> Async.Ignore
            return read_response client })
        |> Async.RunSynchronously
        |> fun resp ->
            Assert.NotEqual("should have non-empty server", "", resp.headers.["Server"])
        )
      ]

// REQUEST

// TODO: parsing first request line
// TODO: parsing method from first line
// TODO: parsing path from first line
// TODO: parsing http version from first line

// TODO: parsing raw request
// TODO: parsing input headers into map

// TODO: missing Host header -> 400 bad request


// RESPONSE

// TODO: receive loop
// TODO: processor
// TODO: call into user's code from processor

// TODO: sending byte []
// TODO: sending Stream
// TODO: using ./mcs/class/Mono.Posix/Mono.Unix/UnixStream.cs:349, sendfile

// TODO: implement multipart messages that send 'multipart/byteranges'

// TODO: for each of the headers, specify what happens if it's invalid

// TODO: write Peach model fuzzer for input parser protocol
// TODO: write Peach model fuzzer for hypermedia protocol*

// TODO: implement HTTP 2.0 http://tools.ietf.org/html/draft-ietf-httpbis-http2-12
// TODO: implement HTTP 2.0 with TLS with http://tools.ietf.org/html/draft-ietf-tls-applayerprotoneg-05
//       https://www.npmjs.org/package/http2
//       https://github.com/http2/http2-spec/wiki/Implementations
// TODO: hpack/header packing for HTTP 2.0

// TODO: support OWIN, have a look at https://github.com/MSOpenTech/http2-katana/blob/master/src/Microsoft.Http2.Owin.Server/HttpSocketServer.cs

// APP

// TODO: applicative for the Host header, if not set, return Bad Request
// TODO: for 'full responses, applicative that gives Content-MD5 header
// TODO: applicative for session handling, feeding user context with data/sid
// TODO: Server-Sent Events https://developer.mozilla.org/en-US/docs/Server-sent_events/Using_server-sent_events
//       http://www.html5rocks.com/en/tutorials/eventsource/basics/
//       http://www.w3.org/TR/2009/WD-eventsource-20091029/
// TODO: HTTP 2.0 handshake, priorities, flowControl, securePort

// HTTP CLIENT

// TODO: implement a HttpClient that just works



// * peach resources;
// http://www.flinkd.org/2011/07/fuzzing-with-peach-part-1/
// http://rockfishsec.blogspot.se/2014/01/fuzzing-vulnserver-with-peach-3.html
// http://old.peachfuzzer.com/v3/TutorialDumbFuzzing/CreateDataModel.html
