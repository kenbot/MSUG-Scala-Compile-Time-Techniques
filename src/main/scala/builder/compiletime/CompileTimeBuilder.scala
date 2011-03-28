package builder.compiletime


class Person private[compiletime](
  val age: Int, 
  val name: String, 
  val languages: List[String]) {
  
  override def toString() = name + ", " + age + ", speaks " + languages.mkString(", ")
}

  
object Person {
  def builder = new PersonBuilder {
    val age, name = None
    val languages = Nil
    type ageCount = Zero
    type nameCount = Zero
    type languagesCount = Zero
  }
  
}

trait Count { type plus1 <: Count }
trait OneOrMany extends Count { type plus1 = Many }
trait Zero extends Count { type plus1 = One }
trait One extends OneOrMany
trait Many extends OneOrMany



object PersonBuilder {

  type CompleteBuilder = PersonBuilder {
    type ageCount = One
    type nameCount = One
    type languagesCount <: OneOrMany
  }

  implicit def enableBuild(b: CompleteBuilder) = new {
    def build = new Person(b.age.get, b.name.get, b.languages)
  }
}

trait PersonBuilder { self =>
  protected val age: Option[Int]
  protected val name: Option[String]
  protected val languages: List[String]

  // Count the number of calls
  type ageCount <: Count
  type nameCount <: Count
  type languagesCount <: Count
  
  
  def withAge(a: Int) = new PersonBuilder {
    val age = Some(a)
    val name = self.name
    val languages = self.languages
    
    type ageCount = self.ageCount#plus1
    type nameCount = self.nameCount
    type languagesCount = self.languagesCount
  }
  
  def withName(n: String) = new PersonBuilder {
    val age = self.age
    val name = Some(n)
    val languages = self.languages
   
    type ageCount = self.ageCount
    type nameCount = self.nameCount#plus1
    type languagesCount = self.languagesCount
  }
  
  def withLanguage(lang: String) = new PersonBuilder {
    val age = self.age
    val name = self.name
    val languages = lang :: self.languages

    type ageCount = self.ageCount
    type nameCount = self.nameCount
    type languagesCount = self.languagesCount#plus1
  }
  
}

