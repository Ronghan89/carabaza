package bbva.normative

import com.typesafe.config.ConfigFactory
import scala.collection.mutable._
import bbva.normative._

object check extends App {
  val dbjson = ConfigFactory.load().getString("my.jsonpath.dbjson")
  val dbverify = dbVerify(dbjson)
  
  val tbjson = ConfigFactory.load().getString("my.jsonpath.tbjson")
  val tbverify = tbVerify(tbjson)
  
  val coljson = ConfigFactory.load().getString("my.jsonpath.coljson")
  val colverify = colVerify(coljson)
  
  val driver = ConfigFactory.load().getString("my.connection.driver")
  val url = ConfigFactory.load().getString("my.connection.url")
  val username = ConfigFactory.load().getString("my.connection.username")
  val password = ConfigFactory.load().getString("my.connection.password")
  var myconnection = new ScalaJdbcConnectSelect(driver, url, username, password)
  var errArray = ArrayBuffer[Tuple2[String, String]]()

  //SHOW databases, resultset.database_name
  val showdbquery = "SHOW databases"
  val databases = myconnection.queryForArray(showdbquery, "database_name").asInstanceOf[ArrayBuffer[String]]
  var tableArray = ArrayBuffer[String]()
  var colArray = ArrayBuffer[Tuple2[String, String]]()

  var dbNames = databases.partition(dbname => dbverify.checkReg(dbname))
  dbNames._2.foreach((dbname: String) => {
    errArray += (dbname -> "error: DDBB name incorrect")
  })
  
  for (some_db <- dbNames._1) {
    //show tables in some_db  resultset.tab_name
    val showtbquery = "SHOW tables in " + some_db
    tableArray = myconnection.queryForArray(showtbquery, "tab_name").asInstanceOf[ArrayBuffer[String]]
    for (tbname <- tableArray) {
      val tab_name = some_db + "." + tbname
      if (tbverify.checkReg(tbname)) {
        //describe database.tablename;   
        //DESCRIBE EXTENDED tbname (TBLPROPERTIES ('transient_lastDdlTime'='1418152638'))
        //owner:cloudera, createTime:1493290195
        //parameters:{transient_lastDdlTime=1493290195}
        //resultset.col_name, data_type
        val desctablequery = "describe " + tab_name
        colArray = myconnection.queryFor2Arrays(desctablequery, ("col_name", "data_type"))
        errArray ++= colverify.checkNameandType(colArray, tab_name)
      } else {
        errArray += (tab_name -> "error: table name incorrect")
      }
    }
  }
  
  myconnection.connection.close()
  //TODO sort errArray
  errArray.foreach(println)

}
