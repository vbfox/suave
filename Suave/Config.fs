module Suave.Config

open System
open System.Net
open System.Threading

open OpenSSL.X509

open Suave.ReqResp
open Suave.Log

type MimeTypesMap = string -> MimeType option

type HttpRuntime =
  { protocol           : Protocol
  ; error_handler      : ErrorHandler
  ; web_part_timeout   : TimeSpan
  ; mime_types_map     : MimeTypesMap
  ; home_directory     : string
  ; compression_folder : string
  ; logger             : Logger }

type Port = uint16

/// A HTTP binding is a protocol is the product of HTTP or HTTP, a DNS or IP binding and a port number
type HttpBinding =
  /// The scheme in use
  { scheme : Protocol
  /// The host or IP address to bind to. This will be interpreted by the operating system
  ; ip     : IPAddress
  /// The port for the binding
  ; port   : Port }
with
  /// Create a HttpBinding for the given protocol, an IP address to bind to and a port
  /// to listen on.
  static member Create(proto, ip : string, port : int) =
    { scheme = proto
    ; ip     = IPAddress.Parse ip
    ; port   = uint16 port }
  /// Overrides the default ToString() method to provide an implementation that is assignable
  /// to a BaseUri for a RestClient/HttpClient.
  override x.ToString() =
    sprintf "%O://%O:%d/" x.scheme x.ip x.port

/// The core configuration of suave. See also Suave.Web.default_config which
/// you can use to bootstrap the configuration:
/// <code>{ default_config with bindings = [ ... ] }</code>
type SuaveConfig =
  /// The bindings for the web server to launch with
  { bindings         : HttpBinding list

  /// An error handler to use for handling exceptions that are
  /// are thrown from the web parts
  ; error_handler    : ErrorHandler

  /// Timeout for responses to be generated from the web part/user code.
  ; web_part_timeout : TimeSpan

  /// Timeout to wait for the socket bind to finish
  ; listen_timeout   : TimeSpan

  /// A cancellation token for the web server. Signalling this token
  /// means that the web server shuts down
  ; ct               : CancellationToken

  /// buffer size for socket operations
  ; buffer_size      : int

  /// max number of concurrent socket operations
  ; max_ops          : int

  /// MIME types
  ; mime_types_map   : MimeTypesMap

  /// Home or root directory
  ; home_folder      : string option

  /// Folder for temporary compressed files
  ; compressed_files_folder : string option

  /// A logger to log with
  ; logger           : Logger }
