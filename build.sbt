val commonSettings = Seq(
  organization := "io.twitchapi4s",
  scalaVersion := "2.12.10",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-Ypartial-unification",
    "-Ywarn-unused:implicits",
    "-Ywarn-unused:imports",
    "-Ywarn-unused:locals",
    "-Ywarn-unused:params",
    "-Ywarn-unused:patvars",
    "-Ywarn-unused:privates"
  ),
  publishMavenStyle := true
)

lazy val root = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := "mtproto4s-core",
    version := "0.0.1",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.0.0",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "org.scalacheck" %% "scalacheck" % "1.14.1" % "test",
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3",
      "dev.zio" %% "zio" % "1.0.0-RC13",
      "dev.zio" %% "zio-streams" % "1.0.0-RC13",
      "org.scala-lang" % "scala-reflect" % "2.12.10"
      // "org.typelevel" %% "cats-mtl-core" % "0.4.0",
      // "org.typelevel" %% "cats-effect" % "1.3.1",
      // "com.softwaremill.sttp" %% "core" % "1.5.17",
      // "io.circe" %% "circe-parser" % "0.10.0",
      // "io.circe" %% "circe-generic" % "0.10.0",
    )
  )

//val commonImplSettings = Seq(
//  version := "0.1.3",
//  libraryDependencies ++= Seq(
//    "org.scalatest" %% "scalatest" % "3.0.5" % "test"
//  ),
//  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
//)
//
//lazy val zioimpl = (project in file("implementations/zio"))
//  .settings(commonSettings: _*)
//  .settings(commonImplSettings: _*)
//  .settings(
//    name := "twitch-zio",
//    libraryDependencies ++= Seq(
//      "org.scalaz" %% "scalaz-zio" % "1.0-RC5",
//      "org.scalaz" %% "scalaz-zio-interop-cats" % "1.0-RC5",
//      "com.softwaremill.sttp" %% "async-http-client-backend-zio" % "1.5.17"
//    )
//  )
//  .dependsOn(root)
//
//lazy val moniximpl = (project in file("implementations/monix"))
//  .settings(commonSettings: _*)
//  .settings(commonImplSettings: _*)
//  .settings(
//    name := "twitch-monix",
//    libraryDependencies ++= Seq(
//      "io.monix" %% "monix" % "3.0.0-RC2",
//      "io.monix" %% "monix-execution" % "3.0.0-RC2",
//      "org.typelevel" %% "cats-effect" % "1.3.1",
//      "com.softwaremill.sttp" %% "async-http-client-backend-monix" % "1.5.17"
//    )
//  )
//  .dependsOn(root)
