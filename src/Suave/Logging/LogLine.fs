namespace Suave.Logging

open System

open Suave

/// When logging, write a log line like this with the source of your
/// log line as well as a message and an optional exception.
type LogLine =
  { /// the trace id and span id
    /// If using tracing, then this LogLine is an annotation to a
    /// span instead of a 'pure' log entry
    trace         : TraceHeader
    /// the level that this log line has
    level         : LogLevel
    /// the source of the log line, e.g. 'ModuleName.FunctionName'
    path          : string
    /// the message that the application wants to log
    message       : string

    data          : Map<string, obj>

    /// an optional exception
    ``exception`` : exn option

    tags          : string Set

    /// timestamp when this log line was created
    timestamp     : DateTimeOffset }
    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogLine =
  let mk path level trace ex data tags message =
    let now = Globals.now ()
    { message       = message
      level         = level
      path          = path
      trace         = trace
      ``exception`` = ex
      data          = data
      tags          = tags
      timestamp     = now }

  let trace_ =
    (fun x -> x.trace),
    fun v (x : LogLine) -> { x with trace = v }

  let level_ =
    (fun x -> x.level),
    fun v (x : LogLine) -> { x with level = v }

  let path_ =
    (fun x -> x.path),
    fun v (x : LogLine) -> { x with path = v }

  let message_ =
    (fun x -> x.message),
    fun v (x : LogLine) -> { x with message = v }

  let exception_ =
    (fun x -> x.``exception``),
    fun v (x : LogLine) -> { x with ``exception`` = v }

  let data_ =
    (fun x -> x.data),
    fun v (x : LogLine) -> { x with data = v }

  let tags_ =
    (fun x -> x.tags),
    fun v (x : LogLine) -> { x with data = v }

  let timestamp_ =
    (fun x -> x.timestamp),
    fun v (x : LogLine) -> { x with timestamp = v }