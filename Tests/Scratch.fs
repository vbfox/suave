module Suave.Scratch

open System
open System.IO

open Fuchu

open NodaTime

module Abstractions =

  module DateTime =
    open System.Globalization

    type RFC1132 = Instant -> string

    type ISO8601 = Instant -> string
    type RFC3339 = ISO8601

    /// an abstraction for the clock
    type Clock =
      abstract Now : unit -> Instant

    let sytem_clock =
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

  module Fs =
    type FilePath = string

    type FileSnapshot =
      { created : Instant
        updated : Instant }

    type FileSystem =
      abstract Open : FilePath -> FileStream * FileSnapshot

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
      | NoCache
      | NoStore
      | MaxAge of int<s>
      | MaxStale of int<s> option
      | MinFresh of int<s>
      | NoTransform
      | OnlyIfCached
      | CacheExt of string

    type CacheRespDir =
      | Public
      | Private of FieldName option
      | NoCache of int<s> option
      | NoStore
      | NoTransform
      | MustRevalidate
      | ProxyRevalidate
      | MaxAge of int<s>
      | SMaxAge of int<s>
      | CacheExtension

    // Connection: request only
    type ConnectionToken =
      | Conn_Close
      | Conn_KeepAlive

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

  /// http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
  type Base64 = string

  type ResponseHeaders =
    | Server of string
    | ``Cache-Control`` of CacheRespDir
    | ``Content-Encoding`` of string
    | ``Content-Language`` of string list // comma sep
    | ``Content-Length`` of uint64 // transfer len of msg body
    | ``Content-Location`` of Uri // rel or abs
    | ``Content-MD5`` of Base64
    | ``Content-Range`` of ContentRangeSpec // with 206 Partial Content
    | ``Content-Type`` of MediaType
    | Date of Instant // RFC822, date always in RFC 1123 format
    | ETag of QuotedString

  module Conneg =
    let i : Instant
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

// TODO: implement multipart messages that send 'multipart/byteranges'

// TODO: for each of the headers, specify what happens if it's invalid

// TODO: write Peach model fuzzer for input parser protocol
// TODO: write Peach model fuzzer for hypermedia protocol*

// TODO: implement HTTP 2.0 http://tools.ietf.org/html/draft-ietf-httpbis-http2-12

// HTTP CLIENT

// TODO: implement a HttpClient that just works

// * peach resources;
// http://www.flinkd.org/2011/07/fuzzing-with-peach-part-1/
// http://rockfishsec.blogspot.se/2014/01/fuzzing-vulnserver-with-peach-3.html
// http://old.peachfuzzer.com/v3/TutorialDumbFuzzing/CreateDataModel.html