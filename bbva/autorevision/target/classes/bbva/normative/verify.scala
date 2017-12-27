package bbva.normative

import scala.io.Source
import java.io.{ FileReader, FileNotFoundException, IOException }
import org.json4s._
import org.json4s.native.JsonMethods._

abstract class Verify {
  def pattern: String
  def checkReg(str: String): Boolean = {
    if (str.matches(pattern))
      true
    else
      false
  }
}
    
trait getPrefix {
  def getPrefix(str: String, sep: String): String = {
    return str.split(sep)(0)
  }
}

case class dbVerify(dbPrefijo: Array[String], symSep: String, maxLength: Int) extends Verify {
  private val dbPreChain = dbPrefijo.map(i => i.toLowerCase()).mkString("|")
  val pattern = ("""^(""" + dbPreChain + ")" + symSep + """([a-z0-9_]){1,""" + maxLength + """}$""")
}

object dbVerify{
  def apply(path: String) = {
    val myjson = Source.fromFile(path).mkString
    val myjv = parse(myjson)
    implicit val formats = DefaultFormats
    val myvfy = myjv.extract[dbVerify]
    myvfy
    }
}
case class tbVerify(symFijo: String, codProject: Array[String], tbPrefijo: Array[String], symSep: String, maxLength: Int) extends Verify {
  private val tbProChain = codProject.map(i => i.toLowerCase()).mkString("|")
  private val tbCapChain = tbPrefijo.map(i => i.toLowerCase()).mkString("|")
  val pattern = "^" + symFijo + "(" + tbProChain + ")(" + tbCapChain + ")" + symSep + "[a-z0-9_]{1," + maxLength + """}$"""
}
object tbVerify{
  def apply(path: String) = {
    val myjson = Source.fromFile(path).mkString
    val myjv = parse(myjson)
    implicit val formats = DefaultFormats
    val myvfy = myjv.extract[tbVerify]
    myvfy
    }
}
case class colVerify(prefixType: Map[String, Array[String]], fixcolname: Map[String, String], symSep: String, maxLength: Int) extends Verify with getPrefix {
  private val colpreArray = prefixType.keySet ++ fixcolname.keySet.map(x => getPrefix(x, "_"))
  private val colPreChain = colpreArray.map(i => i.toLowerCase()).mkString("|")
  val pattern = "^(" + colPreChain + ")" + symSep + "[a-z0-9]{1," + maxLength + "}"
  import scala.collection.mutable._
  def checkNameandType(colArray: ArrayBuffer[Tuple2[String, String]], tbname: String): ArrayBuffer[Tuple2[String, String]] = {
    var resultArray = ArrayBuffer[Tuple2[String, String]]()
    //BEGIN CHECK
    for (x <- colArray) {
      var mynameprefix = getPrefix(x._1, "_")
      if (checkReg(x._1)) {
        //if name is correct
        var mytypeprefix = getPrefix(x._2, """\(""").trim()
        if (prefixType.keySet.contains(mynameprefix)) {
          //if col name prefix is normal
          var typArray = prefixType(mynameprefix)
          var typPreArray = typArray.map(i => getPrefix(i, """\("""))
          
          if (typPreArray.exists(_.equalsIgnoreCase(mytypeprefix))) {
            //if typeprefix is in the array
            if (!(typArray.contains(x._2))) {
              //type correct but length wrong
              resultArray += (tbname + "." + x._1 + " type " + x._2 -> "warning: length not in dominio")
            }
          }else {
            //typeprefix is not included in the array
            resultArray += (tbname + "." + x._1 + " type " + x._2 -> "error: column type incorrect")
          }
        }
      } else if (fixcolname.keySet.contains(x._1)) {
        //if name is a special name eg."aud_fecha"
        if (!fixcolname(x._1).contains(x._2))
          //if name is correct but not in 
          resultArray += (tbname + "." + x._1 + " type " + x._2 -> "error: column type incorrect")
      } else //name is incorrect
        resultArray += (tbname + "." + x._1 -> "error: column name incorrect")
    }
    return resultArray
  }
}
object colVerify{
  def apply(path: String) = {
    val myjson = Source.fromFile(path).mkString
    val myjv = parse(myjson)
    implicit val formats = DefaultFormats
    val myvfy = myjv.extract[colVerify]
    myvfy
    }
}

  
