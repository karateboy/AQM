object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(57); 
  println("Welcome to the Scala worksheet");$skip(35); 
  val fooList = List((1,1), (2,2));System.out.println("""fooList  : List[(Int, Int)] = """ + $show(fooList ));$skip(19); val res$0 = 
  fooList.map{_+_};System.out.println("""res0: <error> = """ + $show(res$0))}
}
