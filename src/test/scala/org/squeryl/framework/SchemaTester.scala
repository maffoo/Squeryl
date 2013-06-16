package org.squeryl.framework

import org.squeryl.{Database, Session, Schema}
import org.squeryl.PrimitiveTypeMode._

import org.scalatest._
import org.scalatest.events.{TestIgnored, Ordinal}
import org.scalatest.matchers.ShouldMatchers

abstract class SchemaTester extends DbTestBase{

  def schema : Schema

  def prePopulate(implicit cs: Session) = {}

  override def beforeAll() {
    super.beforeAll
    if (notIgnored) {
      database.withSession { implicit session =>
        inTransaction {
          schema.drop
          schema.create
          try {
            prePopulate
          } catch {
            case e : Exception =>
              println(e.getMessage)
              println(e.getStackTraceString)
          }
        }
      }
    }
  }

  override def afterAll() {
    super.afterAll
    if (notIgnored) {
      database.withSession { implicit session =>
        transaction {
          schema.drop
        }
      }
    }
  }
}

abstract class DbTestBase extends fixture.FunSuite with ShouldMatchers with BeforeAndAfterAll with BeforeAndAfterEach {

  def connectToDb : Option[Database]

  private var _database: Option[Database] = None
  def notIgnored = _database.isDefined
  def database = _database.get

  val ignoredTests : List[String] = Nil

  type FixtureParam = Session

  def withFixture(test: OneArgTest) {
    database.withSession { session =>
      test(session)
    }
  }

  override def beforeAll() {
    super.beforeAll
    _database = connectToDb
  }

  override def runTest(
    testName: String,
    reporter: Reporter,
    stopper: Stopper,
    configMap: Map[String, Any],
    tracker: Tracker): Unit = {

    if (!notIgnored || ignoredTests.find(_ == testName).isDefined) {
      //reporter(TestIgnored(new Ordinal(0), suiteName, Some(this.getClass.getName),testName))
      return
    }
    super.runTest(testName, reporter, stopper, configMap, tracker)
  }
}

