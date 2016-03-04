package object effectful {

  import language.experimental.macros
  import language.implicitConversions
  import language.higherKinds
  import reflect.macros.blackbox.Context
  import scalaz.{Monad, Unapply}
  import scala.reflect.internal.annotations.compileTimeOnly

  private[effectful] val EFFECTFULLYU = "effectfullyU"
  private[effectful] val UNWRAPU = "unwrapU"
  private[effectful] val EFFECTFUL_TO_UNWRAPPABLE_U = "effectfulToUnwrappableU"

  private[effectful] val EFFECTFULLY = "effectfully"
  private[effectful] val UNWRAP = "unwrap"
  private[effectful] val EFFECTFUL_TO_UNWRAPPABLE = "effectfulToUnwrappable"

  def effectfullyU[M[_], A](expr: A): M[A] = macro effectfullyUImpl[M, A]

  def effectfullyUImpl[M[_], A](c1: Context)(expr: c1.Expr[A]): c1.Expr[M[A]] = {
    val rewriter = new {
      val c: c1.type = c1
    } with Rewriter
    c1.Expr(rewriter.rewrite(expr.tree, rewriter.unapplyTypeclassProvider))
  }

  def effectfully[M[_], A](expr: A): M[A] = macro effectfullyImpl[M, A]

  def effectfullyImpl[M[_], A](c1: Context)(expr: c1.Expr[A]): c1.Expr[M[A]] = {
    val rewriter = new {
      val c: c1.type = c1
    } with Rewriter
    c1.Expr(rewriter.rewrite(expr.tree, rewriter.plainTypeclassProvider))
  }

  @compileTimeOnly(s"Cannot unwrap outside of a `$EFFECTFULLYU` block")
  def unwrapU[MA](expr: MA)(implicit u: Unapply[Monad, MA]): u.A = sys.error(s"$UNWRAPU was not macro'ed away!")

  @compileTimeOnly(s"Cannot unwrap outside of a `$EFFECTFULLY` block")
  def unwrap[M[_], A](expr: M[A])(implicit M: Monad[M]): A = sys.error(s"$UNWRAP was not macro'ed away!")

  implicit def effectfulToUnwrappableU[MA](ma: MA)(implicit U: Unapply[Monad, MA]): Unwrappable[MA, U.A] = new Unwrappable(ma)
  implicit def effectfulToUnwrappable[M[_], A](ma: M[A])(implicit M: Monad[M]): Unwrappable[M[A], A] = new Unwrappable(ma)
}