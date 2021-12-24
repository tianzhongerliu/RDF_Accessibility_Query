import java.io.File
import scala.collection.mutable.Map
import scala.xml.XML

object RDFreader {
  def read(RDF: Map[String, List[Map[String, String]]]): Unit = {
    val A: Map[String, Int] = Map()

    //获取所有文件名
    val path = new File("data\\University_data")
    val filelist = getFile(path)

    //val university = XML.loadFile("data\\University_data\\University50_0.owl")
    //println("全职教授数目： " + (university \\ "FullProfessor").size)

    //println(filelist.size)    991
    for (num <- 1 to filelist.size) {
      //读取所有文件信息
      val university = XML.loadFile(filelist(num - 1))
      var a: Int = 1
      var str: String = ""
      var strlist: List[Map[String, String]] = List()
      for (arr <- university.child) {
        a = 1
        str = arr.label
        if (str != "#PCDATA") {
          if (A.contains(str)) {
            a = A.get(str).get
            a = a + 1
            A += (str -> a)
            strlist = RDF.get(str).get
            val nodemap: Map[String, String] = Map()
            delnode(arr, nodemap)
            strlist = strlist :+ nodemap
            RDF += (str -> strlist)
          } else {
            //第一次读到一个子节点
            A += (str -> a)
            //if(str == "FullProfessor"){
            //val nodemap:Map[String,String] = Map()
            //delnode(arr,nodemap)
            //println(nodemap)
            //}
            var strlist: List[Map[String, String]] = List()
            val nodemap: Map[String, String] = Map()
            delnode(arr, nodemap)
            strlist = strlist :+ nodemap
            RDF += (str -> strlist)
          }
        }
      }
    }
    //println(RDF.get("Lecturer").get)
  }

  //从一个节点中提取信息
  def delnode(node: scala.xml.Node, nodemap: Map[String, String]): Unit = {
    for (m <- node.attributes) {
      if (m.key == "about") {
        nodemap += ("selfurl" -> m.value.text)
      }
    }

    //获得子节点的序列
    var A: List[String] = List()
    var a: Int = 1
    var str: String = ""
    for (arr <- node.child) {
      str = arr.label
      if (str != "#PCDATA") {
        if (A.contains(str)) {
          a += 1
          for (m <- arr.attributes) {
            if (m.key == "about" || m.key == "resource") {
              nodemap += (arr.label + "_" + a.toString -> m.value.text)
            }
          }
        } else {
          //第一次读到一个子节点
          a = 1
          A = A :+ str

          //如果子节点还有子节点，读取子节点信息
          if (arr.child.length > 1) {
            for (arr2 <- arr.child) {
              for (n <- arr2.attributes) {
                if (n.key == "about" || n.key == "resource") {
                  nodemap += (arr.label + "_" + arr2.label -> n.value.text)
                }
              }
            }
          } else if (arr.child.length == 1) {
            nodemap += (arr.label -> arr.text)
          } else {
            for (m <- arr.attributes) {
              if (m.key == "about" || m.key == "resource") {
                nodemap += (arr.label -> m.value.text)
              }
            }
          }
        }
      }
    }
    //println(nodemap)
  }

  def getFile(file: File): Array[File] = {
    var files: Array[File] = Array()
    files = file.listFiles().filter(!_.isDirectory)
      .filter(t => t.toString.endsWith(".owl"))
    files ++ file.listFiles().filter(_.isDirectory).flatMap(getFile)
  }
}
