/// A module for all the request specific and runtime-specific
/// data
module Suave.ReqResp

open System
open System.IO

open OpenSSL.X509 // TODO: remove hard binding to OpenSSL

/// Gets the supported protocols, HTTP and HTTPS with a certificate
type Protocol =
  /// The HTTP protocol is the core protocol
  | HTTP
  /// The HTTP protocol tunneled in a TLS tunnel
  | HTTPS of X509Certificate
with
  override x.ToString() =
    match x with
    | HTTP    -> "http"
    | HTTPS _ -> "https"
  member x.is_secure =
    match x with
    | HTTP    -> false
    | HTTPS _ -> true

/// HTTP cookie
type HttpCookie =
  { name      : string
    value     : string
    expires   : DateTimeOffset option
    path      : string option
    domain    : string option
    secure    : bool
    http_only : bool
    version   : string option }

/// A file's mime type and if compression is enabled or not
type MimeType =
  { name        : string // TODO: split into its segments to allow conneg
  ; compression : bool } // TODO: have by the side, not in the mime type

/// A holder for uploaded file meta-data
type HttpUpload =
  { field_name : string
    file_name  : string
    mime_type  : MimeType
    path       : string }

type HttpVersion =
  | V1_1
  | V2_0
  /// Gets a string representation without the V prefix. E.g. 1.1.
  override x.ToString() =
    match x with
    | V1_1 -> "1.1"
    | V2_0 -> "2.0"

// ref https://github.com/basho/webmachine/blob/develop/include/wm_reqdata.hrl
// TODO: go through all properties and see how well they fit

/// A holder for the data extracted from the request.
type ReqResp =
  { http_version : HttpVersion
    url          : string
    ``method``   : string
    query        : Map<string,string>
    headers      : Map<string,string>
    form         : Map<string,string>
    raw_query    : string
    cookies      : Map<string, (string*string)[]>
    user_name    : string // TODO: move to separate record
    password     : string // TODO: move to separate record
    session_id   : string
    resp_headers : Map<string, string>
    files        : HttpUpload list
    trace        : Log.TraceHeader
    protocol     : Protocol
    /// The raw request body as a byte array
    raw_body     : Lazy<byte []> }

type UserContext = Map<string, string>

/// An error handler takes the exception, a programmer-provided message, a request (that failed) and returns
/// an asynchronous workflow for the handling of the error.
type ErrorHandler = exn -> String -> ReqResp -> Async<unit>

type HttpContext = ReqResp * UserContext

//  { request    : ReqData
//  ; runtime    : HttpRuntime
//  ; connection : Connection }

type Applicative = HttpContext -> HttpContext option

/// A web part is a thing that executes on a HttpRequest, asynchronously, maybe executing
/// on the request.
///
/// Note: WebResult = Async<unit> option
type WebPart = HttpContext -> Async<unit> option

/// An exception, raised e.g. if writing to the stream fails
exception internal InternalFailure of string

module Req =
  //  let request f (a : HttpContext) = f a.request a
  /// Gets the query from the HttpRequest
  let query (x : ReqData) = x.query
  /// Gets the form from the HttpRequest
  let form  (x : ReqData) = x.form

module Resp =
  ()
