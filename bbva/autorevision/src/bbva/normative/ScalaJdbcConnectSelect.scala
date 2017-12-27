package bbva.normative
import java.sql.SQLException;
import scala.collection.mutable._;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.sql.DriverManager;

class ScalaJdbcConnectSelect(val driver: String, val url: String, val username: String, val password: String) {
  var connection: Connection = null
  // make the connection
  try {
    // make the connection
    Class.forName(driver)
    connection = DriverManager.getConnection(url, username, password)
  } catch {
    case e => e.printStackTrace
  }

  def queryForArray(query: String, fields: String): ArrayBuffer[_ <: Any] = {
    var dbArray = ArrayBuffer[String]()
    try {
      // create the statement, and run the select query
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(query)
      while (resultSet.next()) {
        dbArray += resultSet.getString(fields)
      }
    } catch {
      case e => e.printStackTrace
    }
    dbArray
  }
  def queryFor2Arrays(query: String, fields: Tuple2[String,String]): ArrayBuffer[Tuple2[String,String]] = {
    var dbArray = ArrayBuffer[Tuple2[String,String]]()
    try {
      // create the statement, and run the select query
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(query)
      while (resultSet.next()) {
        dbArray += (resultSet.getString(fields._1)-> resultSet.getString(fields._2))
      }
    } catch {
      case e => e.printStackTrace
    }
    dbArray
  }
  def queryForFirstValue(query: String, fields: String): Any = {
    try {
      // create the statement, and run the select query
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(query)
      if(resultSet.next()) {
        return resultSet.getString(fields)
      }
    } catch {
      case e => e.printStackTrace
    }
  }
}