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
scalaVersion := "2.12.14"
crossScalaVersions := Seq("2.12.14", "2.13.6")
exportJars := true
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")
testOptions += Tests.Argument("-oDF")
publishMavenStyle := true
homepage := Some(url("http://scalawag.org/sarong"))
startYear := Some(2021)
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.8",
) map (_ % "test")

publishTo := Some(Resolver.file("Not actually used but required by publish-signed", file("/tmp/bogusrepo")))