// sarong -- Copyright 2021 -- Justin Patterson
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

organization := "org.scalawag.sarong"
scalaVersion := "2.13.17"
crossScalaVersions := Seq("2.12.19", "2.13.17", "3.3.7")

Compile / unmanagedSourceDirectories ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => Seq((Compile / sourceDirectory).value / "scala-2")
    case Some((3, _)) => Seq((Compile / sourceDirectory).value / "scala-3")
    case _ => Nil
  }
}

Test / unmanagedSourceDirectories ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => Seq((Test / sourceDirectory).value / "scala-2")
    case Some((3, _)) => Seq((Test / sourceDirectory).value / "scala-3")
    case _ => Nil
  }
}

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")
    case Some((3, _)) => Seq("-deprecation", "-feature", "-language:implicitConversions")
    case _ => Nil
  }
}

testOptions += Tests.Argument("-oDF")
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19",
) map (_ % "test")

ThisBuild / versionScheme := Some("semver-spec")
