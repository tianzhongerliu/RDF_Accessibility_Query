import Query.{FindConnect, ReadQuery}
import RDFreader.read

import scala.collection.mutable.Map
import scala.io.{BufferedSource, Source}

object main {
  def main(args: Array[String]) {
    //存储所有字节点及其属性信息
    val RDF: Map[String, List[Map[String, String]]] = Map()
    read(RDF)
    //println(RDF)
    //println(RDF.get("FullProfessor").get)

    //查询
    println("SELECT查询结果：")
    ReadQuery(RDF)
    println()

    //可达性查询
    val source:BufferedSource = Source.fromFile("data\\Accessibility_Query.txt")
    val lines:Iterator[String] = source.getLines()
    val list1:List[String] = lines.toList
    source.close()

    var url1: String = ""
    var url2: String = ""
    for(l <- list1){
      url1 = l.split(",")(0)
      url2 = l.split(",")(1)
      println("Accessibility_Query结果如下：")
      FindConnect(url1,url2,RDF)
    }
    //FindConnect("http://www.Department2.University50.edu/Lecturer2", "http://www.Department2.University50.edu/GraduateCourse50", RDF)
  }

}
