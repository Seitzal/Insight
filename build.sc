import mill._
import scalalib._

  val globalScalaVersion = "2.12.7"
  val globalScalacOptions = Seq[String](
  //  "-deprecation"
  )

trait TestEnv extends TestModule {
  def ivyDeps = Agg(
    ivy"org.scalatest::scalatest::3.0.5",
    ivy"org.scalactic::scalactic::3.0.5"
  )
  def testFrameworks = Seq("org.scalatest.tools.Framework")
}

object core extends ScalaModule {
  def scalaVersion = globalScalaVersion
  def scalacOptions = globalScalacOptions
  
  object test extends Tests with TestEnv
}

object client extends ScalaModule {
  def scalaVersion = globalScalaVersion
  def scalacOptions = globalScalacOptions
  def moduleDeps = Seq(core)
}

object server extends ScalaModule {
  def scalaVersion = globalScalaVersion
  def scalacOptions = globalScalacOptions
  def moduleDeps = Seq(core)
  def ivyDeps = Agg(
    ivy"com.lihaoyi::cask:0.1.9"
  )
}

def t_core() = core.test.test()

def TEST(cmd : => (String, Seq[TestRunner.Result])) {
  print(cmd._1)
}