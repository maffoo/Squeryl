package org.squeryl.test

import org.squeryl.framework.DbTestBase
import org.squeryl.Session

abstract class ConnectionClosingTest extends DbTestBase {

  def dbSpecificSelectNow: String

  test("Closing statement should close connection"){ implicit s =>
    val session = newSession()
    val stmt = session.connection.prepareStatement(dbSpecificSelectNow)
    val rs = stmt.executeQuery

    session.connection.close

    stmt.isClosed should equal(false)
    rs.isClosed should equal(false)
  }
}