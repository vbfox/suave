namespace Suave.Logging

open System

/// The primary Logger abstraction that you can log data into
type Logger =
  abstract Verbose : (unit -> LogLine) -> unit
  abstract Debug : (unit -> LogLine) -> unit
  abstract Log : LogLine -> unit

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Logger =
  let debug (logger : Logger) f_line =
    logger.Debug f_line
  let verbose (logger : Logger) f_line =
    logger.Verbose f_line
  let log (logger : Logger) (line : LogLine) =
    logger.Log line

module ReqCoalescor =
  open Suave.Utils

  type SpanData = LogLine list

  type State =
    { data : Map<ReqId, SpanData>
    }

  let empty = { data = Map.empty }

  let start (state : State) req_id log_line =
    match state.data |> Map.tryFind req_id with
    | None ->
      { state with data = state.data |> Map.add req_id [log_line] }
    | Some _ ->
      // warn here, 1/2^128 probability this state is reached
      { state with data = state.data |> Map.put req_id [log_line] }

  let append (state : State) req_id log_line =
    match state.data |> Map.tryFind req_id with
    | None ->
      failwithf "internal state error, when calling append, needs to have trace_id existing in dictionary"
    | Some lines ->
      { state with data = state.data |> Map.put req_id (log_line :: lines) }

  let finalise (state : State) req_id log_line =
    match state.data |> Map.tryFind req_id with
    | None ->
      failwithf "internal state error, when calling finalise, needs to have trace_id existing in dictionary"
    | Some lines ->
      log_line :: lines, { state with data = state.data |> Map.remove req_id }

  type SpanMatch =
    | NoSpan
    /// span annotation
    | SpanAnn of TraceId
    | SpanServerRecv of TraceId
    | SpanServerSend of TraceId
    // we don't care about client-send/client-recv, until we're done with the proxy

  let match_req = function
    | { trace = t } when t = TraceHeader.empty ->
      NoSpan
    | { tags = ts; trace = { req_id = rid } } when ts |> Set.contains TraceHeader.ServerRecv ->
      SpanServerRecv rid
    | { tags = ts; trace = { req_id = rid } } when ts |> Set.contains TraceHeader.ServerSend ->
      SpanServerSend rid
    | { trace = { req_id = rid } } ->
      SpanAnn rid

  type SpanResult =
    | SpanNext of State
    | SpanClosed of ReqId * State * SpanData

  let handle state line =
    match match_req line with
    | NoSpan ->
      SpanNext state
    | SpanAnn req_id ->
      SpanNext (append state req_id line)
    | SpanServerRecv req_id ->
      SpanNext (start state req_id line)
    | SpanServerSend req_id ->
      let d, st= finalise state req_id line
      SpanClosed (req_id, st, d)

  let should_log level line_level =
    level >= line_level

  let next target state line =
    match handle state line with
    | SpanNext state' ->
      state'
    | SpanClosed (req_id, state', lines) ->
      target (req_id, lines |> Array.ofList)
      state'

  /// mutable, thread safe, coalescing logger that calls back with an
  /// array of logline
  type internal SCL(level : LogLevel, request_done : ReqId * LogLine array -> unit) =
    let mutable state = empty
    let locker = obj()
    interface Logger with
      member x.Verbose f_line =
        if should_log LogLevel.Verbose level then
          lock locker (fun () -> state <- next request_done state (f_line ()))
      member x.Debug f_line =
        if should_log LogLevel.Debug level then
          lock locker (fun () -> state <- next request_done state (f_line ()))
      member x.Log line =
        if should_log line.level level then
          lock locker (fun () -> state <- next request_done state line)

  let mk level request_done = SCL (level, request_done) :> Logger

module Loggers =
  open System.Net
  open System.Collections.Generic

  let NoopLogger =
    { new Logger with
        member x.Verbose _ = ()
        member x.Debug _ = ()
        member x.Log _ = () }

  /// A logger to use for combining a number of other loggers
  type CombiningLogger(other_loggers : Logger list) =
    interface Logger with
      member x.Verbose f_line =
        other_loggers |> List.iter (fun logger -> logger.Verbose f_line)
      member x.Debug f_line =
        other_loggers |> List.iter (fun logger -> logger.Debug f_line)
      member x.Log line =
        other_loggers |> List.iter (fun l -> l.Log line)

  /// let the ISO8601 love flow
  let internal default_formatter (line : LogLine) =
    // [I] 2014-04-05T12:34:56Z: Hello World! [my.sample.app]
    "[" + Char.ToUpperInvariant(line.level.ToString().[0]).ToString() + "] " +
    (DateTime(line.timestamp.Ticks, DateTimeKind.Utc).ToString("o")) + ": " +
    line.message + " [" + line.path + "]" +
    (match line.``exception`` with | Some e -> " exn:\n" + e.ToString() | None -> "")

  /// Log a line with the given format, printing the current time in UTC ISO-8601 format
  /// and then the string, like such:
  /// '2013-10-13T13:03:50.2950037Z: today is the day'
  ///
  /// Ensure that you only create one instance of this type, or the console
  /// colours will be mixed together.
  type ConsoleLogger(min_level, ?formatter, ?colourise, ?original_color, ?console_semaphore) =
    let sem            = defaultArg console_semaphore (obj())
    let original_color = defaultArg original_color Console.ForegroundColor
    let formatter      = defaultArg formatter default_formatter
    let colourise      = defaultArg colourise true
    let write          = System.Console.WriteLine : string -> unit

    let to_color = function
      | LogLevel.Verbose -> ConsoleColor.DarkGreen
      | LogLevel.Debug   -> ConsoleColor.Green
      | LogLevel.Info    -> ConsoleColor.White
      | LogLevel.Warn    -> ConsoleColor.Yellow
      | LogLevel.Error   -> ConsoleColor.DarkRed
      | LogLevel.Fatal   -> ConsoleColor.Red

    let log color line =
      if colourise then
        lock sem <| fun _ ->
          Console.ForegroundColor <- color
          (write << formatter) line
          Console.ForegroundColor <- original_color
      else
        // we don't need to take another lock, since Console.WriteLine does that for us
        (write << formatter) line

    interface Logger with
      member x.Verbose f_line =
        if LogLevel.Verbose >= min_level then
          let line = f_line ()
          log (to_color line.level) line
      member x.Debug f_line =
        if LogLevel.Debug >= min_level then
          let line = f_line ()
          log (to_color line.level) line
      member x.Log line =
        if line.level >= min_level then log (to_color line.level) line
      
  type OutputWindowLogger(min_level, ?formatter) =
    let formatter = defaultArg formatter default_formatter
    let log line = System.Diagnostics.Debug.WriteLine(formatter line)
    interface Logger with
      member x.Verbose f_line =
        if LogLevel.Verbose >= min_level then log (f_line ())
      member x.Debug f_line =
        if LogLevel.Debug >= min_level then log (f_line ())
      member x.Log line =
        if line.level >= min_level then log line
