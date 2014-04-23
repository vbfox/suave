module Suave.Config

open System
open System.Threading
open OpenSSL.X509

open Suave.ReqResp

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
  ; logger           : Log.Logger }
