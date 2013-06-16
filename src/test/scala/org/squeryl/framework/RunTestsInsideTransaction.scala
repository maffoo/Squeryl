package org.squeryl.framework

import org.squeryl.PrimitiveTypeMode.{transaction, using}
import org.squeryl.Session

trait RunTestsInsideTransaction extends DbTestBase {

  override def withFixture(test: OneArgTest) {
    using(newSession()) { implicit session =>
      transaction {
        test(session)
        session.connection.rollback // rollback so each test starts in a clean state
      }
    }
  }

}

