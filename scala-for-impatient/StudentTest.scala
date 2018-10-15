object StudentTest extends App{

  import scala.beans.BeanProperty

  class Student(@BeanProperty var name: String,
                @BeanProperty var id: Long)

}
