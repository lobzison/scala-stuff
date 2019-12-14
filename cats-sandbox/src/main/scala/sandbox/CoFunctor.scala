package sandbox

object CoFunctorApp {
    // trait Printable[A] {self =>
    //     def format(value: A): String
    //     def contramap[B](func: B => A): Printable[B] =
    //         new Printable[B] {
    //         def format(value: B): String =
    //             self.format(func(value))
    //         }
    // }
    
    // def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

    // implicit val stringPrintable: Printable[String] = (value: String) => s"\\${value}\\"
    // implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) => if (value) "yes" else "no"
    
    // println(format("Hallo"))
    // println(format(true))

    // final case class Box[A](value: A)

    // implicit def boxPrintable[A: Printable]: Printable[Box[A]] = 
    //     implicitly[Printable[A]].contramap(x => x.value)

    // println(format(Box("hi")))
    // println(format(Box(false)))

    // trait Codec[A] {self => 
    //     def encode(value: A): String
    //     def decode(value: String): A
    //     def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    //         def decode(value: String): B = dec(self.decode(value))
    //         def encode(value: B): String = self.encode(enc(value))
    //     }
    // }
    // def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
    // def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

    // implicit val stringCodec: Codec[String] =
    //     new Codec[String] {
    //         def encode(value: String): String = value
    //         def decode(value: String): String = value
    //     }

    // // implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)
    // // implicit val boolCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)
    // implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

    // // println(encode(1))
    // // println(encode(true))
    // // println(decode("true"))
    // println(decode[Double]("1.23"))
    // implicit def boxCoded[A: Codec]: Codec[Box[A]] = {
    //     val c = implicitly[Codec[A]]
    //     stringCodec.imap(x => Box(c.decode(x)), x => c.encode(x.value))
    // }

    import cats.Contravariant
    import cats.Show
    import cats.instances.string._
    import cats.syntax.contravariant._

    val showString = Show[String]

    val showSymbol = Contravariant[Show]
        .contramap(showString)((sym: Symbol) => s"'${sym.name}")

    println(showString.contramap[Symbol](_.name).show('dave))
    println('dave)

    import cats.Monoid
    import cats.syntax.invariant._
    import cats.syntax.semigroup._

    implicit val symbolMonoid: Monoid[Symbol] =
        Monoid[String].imap[Symbol](Symbol.apply)(_.name)

    println(Monoid[Symbol].empty)
    println('a |+| 'few |+| 'words)
}