import scala.collection.mutable.Map
import scala.io.Source

object Query {
  def ReadQuery(RDF: Map[String, List[Map[String, String]]]): Unit = {
    //读取实体名称，根据url查询是否有相关关系
    val queryfile = Source.fromFile("data\\query.txt")
    val lines = queryfile.getLines()

    var select_th: String = ""
    var from_url: String = ""
    var url_type: String = ""
    var about: String = ""

    for (line <- lines) {
      //找到要匹配的属性
      if (line.contains("SELECT")) {
        for (l <- line.split(" ")) {
          if (l.contains("?"))
            select_th = l.substring(1)
        }
      }
      //找到作用的域
      if (line.contains("{")) {
        val part = line.split(" ")
        val u = part(0).split("<|>")
        from_url = u(1)
        url_type = part(3).substring(1)
      }
      //找到属性的名字
      if (line.contains("FILTER")) {
        val l = line.split("\"")
        about = l(1)
      }
    }

    //在RDF中查找匹配
    val findlist = RDF.get("FullProfessor").get
    findlist.foreach(s => Pick(about, s, from_url, select_th))
  }

  //查找两个节点之间是否有关系
  def FindConnect(url1: String, url2: String, RDF: Map[String, List[Map[String, String]]]): Unit = {
    //创建两层衍生结构（牺牲了精度，应该可以取得一定效果）
    var url1_Derivative: List[String] = List()
    for (m <- DerivativeUrl(url1, RDF)) {
      if (m.contains("http://")) {
        val m_list = DerivativeUrl(m, RDF)
        if (m_list.size != 0) {
          for (m_list_c <- m_list) {
            val str = m + " " + m_list_c
            url1_Derivative = url1_Derivative :+ str
          }
        } else {
          val str = m + " null"
          url1_Derivative = url1_Derivative :+ str
        }
      } else {
        val str = m + " null"
        url1_Derivative = url1_Derivative :+ str
      }
    }

    var url2_Derivative: List[String] = List()
    for (n <- DerivativeUrl(url2, RDF)) {
      if (n.contains("http://")) {
        val n_list = DerivativeUrl(n, RDF)
        if (n_list.size != 0) {
          for (n_list_c <- n_list) {
            val str = n + " " + n_list_c
            url2_Derivative = url2_Derivative :+ str
          }
        } else {
          val str = n + " null"
          url2_Derivative = url2_Derivative :+ str
        }
      } else {
        val str = n + " null"
        url2_Derivative = url2_Derivative :+ str
      }
    }

    //println(url1_Derivative)
    //println(url2_Derivative)

    //遍历查找相同部分
    for (m <- url1_Derivative) {
      val m_part = m.split(" ")
      for (n <- url2_Derivative) {
        val n_part = n.split(" ")

        //随便写了。。。
        if (url1 == url2) {
          println(url1 + " equal " + url2)
        } else if (m_part(0) == url2) {
          println(url1 + " -> " + m_part(0) + " equal " + url2)
        } else if (m_part(1) == url2) {
          println(url1 + " -> " + m_part(0) + " -> " + m_part(1) + " equal " + url2)
        } else if (n_part(0) == url1) {
          println(url1 + " equal " + url2 + " -> " + n_part(0))
        } else if (n_part(1) == url1) {
          println(url1 + " equal " + url2 + " -> " + n_part(0) + " -> " + n_part(1))
        } else if (n_part(0) == m_part(0) && m_part(0) != "null") {
          println(url1 + " -> " + m_part(0) + " equal " + url2 + " -> " + n_part(0))
        } else if (n_part(0) == m_part(1) && m_part(1) != "null") {
          println(url1 + " -> " + m_part(0) + " -> " + m_part(1) + " equal " + url2 + " -> " + n_part(0))
        } else if (n_part(1) == m_part(0) && m_part(0) != "null") {
          println(url1 + " -> " + m_part(0) + " equal " + url2 + " -> " + n_part(0) + " -> " + n_part(1))
        } else if (n_part(1) == m_part(1) && m_part(1) != "null") {
          println(url1 + " -> " + m_part(0) + " -> " + m_part(1) + " equal " + url2 + " -> " + n_part(0) + " -> " + n_part(1))
        }
      }
    }
  }

  def DerivativeUrl(url: String, RDF: Map[String, List[Map[String, String]]]): List[String] = {
    //输出url衍生的集合
    var urlset: List[String] = List()
    for (node_name <- RDF.keySet) {
      val findlist = RDF.get(node_name).get
      findlist.foreach(s =>
        if (s.get("selfurl").get == url) {
          //println(s)
          for (m <- s.keySet) {
            if (!m.equals("selfurl")) {
              urlset = urlset :+ s.get(m).get
            }
          }
          return urlset
        }
      )
    }
    return urlset
  }

  def Pick(about: String, s: Map[String, String], from_url: String, select_th: String): Unit = {
    if (s.get("selfurl").get == from_url) {
      for (m <- s.keySet) {
        //println(m)
        if (m.contains(select_th) && s.get(m).get.equals(about)) {
          println(about)
          return
        }
      }
      println("doesn't exist")
    }
  }
}
