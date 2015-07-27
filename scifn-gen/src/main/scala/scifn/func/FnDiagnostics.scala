package scifn.func

final case class FnDiagnostics[-A](missing: List[Basis[A, Any]], errors: List[Basis[A, Any]])
