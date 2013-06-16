package org.squeryl.framework

import org.scalatest.matchers.ShouldMatchers
import org.squeryl.{Session, Schema}

import org.squeryl.PrimitiveTypeMode._
import org.scalatest._
import org.scalatest.events.{TestIgnored, Ordinal}

abstract class SchemaTester extends DbTestBase{

  def schema : Schema

  def prePopulate(implicit cs: Session) = {}

  override def beforeAll() {
    super.beforeAll
    if (notIgnored) {
      using(newSession()) { implicit session =>
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
      using(newSession()) { implicit session =>
        transaction {
          schema.drop
        }
      }
    }
  }
}

abstract class DbTestBase extends fixture.FunSuite with ShouldMatchers with BeforeAndAfterAll with BeforeAndAfterEach {

  def connectToDb : Option[() => Session]

  private var sessionFactory: Option[() => Session] = None
  def notIgnored = sessionFactory.isDefined
  def newSession() = sessionFactory.get()

  val ignoredTests : List[String] = Nil

  type FixtureParam = Session

  def withFixture(test: OneArgTest) {
    using(newSession()) { session =>
      test(session)
    }
  }

  override def beforeAll() {
    super.beforeAll
    sessionFactory = connectToDb
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

