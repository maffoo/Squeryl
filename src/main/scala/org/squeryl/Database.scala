package org.squeryl

import PrimitiveTypeMode.transaction

abstract class Database {
  def createSession(): Session

  /**
   * Create a new database session and run the given block with the
   * session passed as a parameter, in autocommit mode. After the
   * block completes, the session will be closed.
   */
  def withSession[A](a: Session => A): A = {
    val session = createSession()
    session.connection.setAutoCommit(true)
    try a(session) finally session.close
  }

  /**
   * Create a new database session and run the given block with the
   * session passed as a parameter, and the entire block running
   * inside a single transaction.
   */
  def withTransaction[A](a: Session => A): A = {
    withSession { implicit session =>
      transaction {
        a(session)
      }
    }
  }
}

object Database {
  def apply(f: => Session) = new Database {
    def createSession() = f
  }
}
