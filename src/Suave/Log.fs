namespace Suave

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Log =
  open Suave.Logging

  let logger : Suave.Logging.Logger option ref =
    ref None

  let verbose (logger : Logger) path message =
    logger.Log (LogLine.mk path LogLevel.Verbose TraceHeader.empty None Map.empty message)

  let verbosef logger path f_format =
    f_format (Printf.kprintf (verbose logger path))

  let verbosee (logger : Logger) path ex message =
    logger.Log (LogLine.mk path LogLevel.Verbose TraceHeader.empty (Some ex) Map.empty message)

  let debug (logger : Logger) path message =
    logger.Log (LogLine.mk path LogLevel.Debug TraceHeader.empty None Map.empty message)

  let debugf logger path f_format =
    f_format (Printf.kprintf (debug logger path))

  let debuge (logger : Logger) path ex message =
    logger.Log (LogLine.mk path LogLevel.Debug TraceHeader.empty (Some ex) Map.empty message)

  let info (logger : Logger) path trace message =
    logger.Log (LogLine.mk path LogLevel.Info trace None Map.empty message)

  let infof logger path trace f_format =
    f_format (Printf.kprintf (info logger path trace))

  let infoe (logger : Logger) path trace ex message =
    logger.Log (LogLine.mk path LogLevel.Info trace (Some ex) Map.empty message)

  let log (logger : Logger) path level msg =
    logger.Log (LogLine.mk path level TraceHeader.empty None Map.empty msg)
