package org.squeryl.mysql

import org.squeryl.test._
import org.squeryl.adapters.MySQLInnoDBAdapter
import org.squeryl.framework.DBConnector
import org.squeryl.Session

trait MySQL_Connection extends DBConnector {
  def connectToDb() : Option[() => Session] = {
    if (config.hasProps("mysql.connectionString")) {
      Class.forName("com.mysql.jdbc.Driver")

      Some(() => {
        val c = java.sql.DriverManager.getConnection(
          config.getProp("mysql.connectionString")
        )
        c.setAutoCommit(false)
        Session.create(c, new MySQLInnoDBAdapter)
      })
    } else {
      None
    }
  }
}

class MySQL_UuidTests extends UuidTests with MySQL_Connection
class MySQL_NestedLeftOuterJoinTest extends NestedLeftOuterJoinTest with MySQL_Connection
class MySQL_SchoolDbMutableRelations extends mutablerelations.SchoolDb2MetableRelations with MySQL_Connection
class MySQL_TransactionTests extends TransactionTests with MySQL_Connection
class MySQL_SchoolDb2 extends schooldb2.SchoolDb2Tests with MySQL_Connection
//class MySQL_SchoolDb extends schooldb.SchoolDbTestRun with MySQL_Connection {
//  override val ignoredTests = List("OuterJoinMixed1")
//} // XXX: these tests fail due to bad DDL when creating schema
//class MySQL_TestCustomTypesMode extends customtypes.TestCustomTypesMode with MySQL_Connection
class MySQL_KickTheTires extends demo.KickTheTires with MySQL_Connection
class MySQL_MusicDb extends musicdb.MusicDbTestRun with MySQL_Connection {
  override val ignoredTests = List("PaginatedQuery1", "OuterJoinMixed1")
}
class MySQL_LeftJoinTest extends LeftJoinTest with MySQL_Connection

//class MySQL_ConnectionClosing extends ConnectionClosingTest with MySQL_Connection {
//  def dbSpecificSelectNow: String = "SELECT CURRENT_TIMESTAMP"
//}
