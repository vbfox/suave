module Suave.ReqResp

open System
open System.IO

/// HTTP cookie
type HttpCookie =
  { name      : string
  ; value     : string
  ; expires   : DateTimeOffset option
  ; path      : string option
  ; domain    : string option
  ; secure    : bool
  ; http_only : bool
  ; version   : string option }

/// A file's mime type and if compression is enabled or not
type MimeType =
  { name        : string
  ; compression : bool }

type MimeTypesMap = string -> MimeType option

/// A holder for headers for the http response
type HttpResponse =
  { headers : Map<string, string> }

/// A holder for uploaded file meta-data
type HttpUpload =
  { field_name : string
  ; file_name  : string
  ; mime_type  : string
  ; path       : string }

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
type ReqData =
  { http_version : HttpVersion
  ; url          : string
  ; ``method``   : string
  ; query        : Map<string,string>
  ; headers      : Map<string,string>
  ; form         : Map<string,string>
  ; raw_form     : byte []
  ; raw_query    : string
  ; cookies      : Map<string, (string*string)[]>
  ; user_name    : string
  ; password     : string
  ; session_id   : string
  ; response     : HttpResponse
  ; files        : HttpUpload list
  ; trace        : Log.TraceHeader
  ; is_secure    : bool }

type HttpContext =
  { request    : ReqData
  ; runtime    : HttpRuntime
  ; connection : Connection }

type HttpRuntime =
  { protocol           : Protocol
  ; web_part_timeout   : TimeSpan
  ; error_handler      : ErrorHandler
  ; mime_types_map     : MimeTypesMap
  ; home_directory     : string
  ; compression_folder : string
  ; logger             : Log.Logger }

/// An error handler takes the exception, a programmer-provided message, a request (that failed) and returns
/// an asynchronous workflow for the handling of the error.
type ErrorHandler = Exception -> String -> HttpContext -> Async<unit>

type WebResult = Async<unit> option

type WebPart = HttpContext -> WebResult

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
