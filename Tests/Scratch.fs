module Suave.Scratch

open System
open System.IO

open Fuchu

open NodaTime

module Abstractions =

  module Fs =
    type FilePath = string

    type FileMetadataSnapshot =
      { created : Instant
        updated : Instant }

    type FileSystem =
      abstract Open : FilePath -> FileStream * FileMetadataSnapshot

  module Logging =

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
      { trace         : TraceHeader
        level         : LogLevel
        path          : string
        message       : string
        ``exception`` : exn option
        ts_utc_ticks  : int64 }

    type Logger =
      abstract member Log : LogLevel -> (unit -> LogLine) -> unit

module Http =
  open Abstractions.Logging

  type HttpProtocol =
    | HTTP
    | HTTPS

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

  /// http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
  type RequestHeaders =
    | Accept
    | TODO

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

  /// A writer has no task other than modifying the state going forward.
  type Writer = HttpContext -> HttpContext

  /// <summary><para>
  /// A web part is a thing that executes on a HttpRequest, asynchronously, maybe executing
  /// on the request.
  /// <para></para>
  /// You can do (:Applicative) >>= (:WebPart), because Applicative returns HttpContext option
  /// and WebPart takes HttpContext and is called if the return value from the applicative
  /// is Some value.
  /// </para></summary>
  type WebPart = HttpContext -> Async<unit> option

  /// An error handler takes the exception, a programmer-provided message, a request (that failed) and returns
  /// an asynchronous workflow for the handling of the error.
  type ErrorHandler = exn -> String -> WebPart

  type Applicative = HttpContext -> HttpContext option

[<Tests>]
let parsing_lines =
  testList "" []

// REQUEST

// TODO: parsing first request line
// TODO: parsing method from first line
// TODO: parsing path from first line
// TODO: parsing http version from first line

// TODO: parsing raw request
// TODO: parsing input headers into map

// RESPONSE

// TODO: receive loop
// TODO: processor
// TODO: call into user's code from processor

// TODO: sending byte []
// TODO: sending Stream
// TODO: using ./mcs/class/Mono.Posix/Mono.Unix/UnixStream.cs:349, sendfile