package builder.compiletime


object Main {

  def main(args: Array[String]) {
    val bob = (Person.builder 
      withAge 17 
      withName "Jim" 
      withLanguage "English" 
      withLanguage "French"
    ).build

    println(bob)
  
  }


}