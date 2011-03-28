package smart


sealed trait Natural {
    val value: Int

    def +(n: Natural) = Natural.unsafeNatural(value + n.value)

    override def toString() = value.toString
}

object Natural {
    // Note private. Only useful for our internal code
    private def unsafeNatural(n: Int): Natural = new Natural {val value = n}

    def natural(n: Int): Option[Natural] = if (n >= 0) Some(unsafeNatural(n))
                                           else None
                                                       
    def naturalOr(n: Int, nat: => Natural) = natural(n) getOrElse nat

} 