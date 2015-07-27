package scifn.func

final case class FnDiagnosticsException(fnDiagnostics: FnDiagnostics[Nothing], cause: Throwable)
extends RuntimeException(cause)
