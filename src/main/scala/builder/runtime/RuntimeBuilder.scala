package builder.runtime



class Person private[runtime](
  val age: Int, 
  val name: String, 
  val languages: List[String]) {
  
  override def toString() = name + ", " + age + ", speaks " + languages.mkString(", ")
}

  
object Person {
  def builder = new PersonBuilder {
    val age, name = None
    val languages = Nil
    val ageCount = Zero
    val nameCount = Zero
    val languagesCount = Zero
  }
}

trait Count {def plus1: Count}
trait OneOrMany extends Count {def plus1 = Many}
object Zero extends Count {def plus1 = One}
object One extends OneOrMany
object Many extends OneOrMany



trait PersonBuilder { self =>
  protected val age: Option[Int]
  protected val name: Option[String]
  protected val languages: List[String]

  // Count the number of calls
  def ageCount: Count
  def nameCount: Count
  def languagesCount: Count
  
  
  def withAge(a: Int) = new PersonBuilder {
    val age = Some(a)
    val name = self.name
    val languages = self.languages
    
    def ageCount = self.ageCount.plus1
    def nameCount = self.nameCount
    def languagesCount = self.languagesCount
  }
  
  def withName(n: String) = new PersonBuilder {
    val age = self.age
    val name = Some(n)
    val languages = self.languages
   
    def ageCount = self.ageCount
    def nameCount = self.nameCount.plus1
    def languagesCount = self.languagesCount
  }
  
  def withLanguage(lang: String) = new PersonBuilder {
    val age = self.age
    val name = self.name
    val languages = lang :: self.languages

    def ageCount = self.ageCount
    def nameCount = self.nameCount
    def languagesCount = self.languagesCount.plus1
  }


  // Finally build the person
  def build: Person = {
    require(ageCount == One)
    require(nameCount == One)
    require(languagesCount == One || languagesCount == Many)
    
    new Person(age.get, name.get, languages)
  }
}

