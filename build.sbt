libraryDependencies  ++=  Seq(
	"org.squeryl" %% "squeryl" % "0.9.5-7",
	posgresDriver
)

// LUCENE
//libraryDependencies += "org.apache.lucene" % "lucene-core" % "5.0.0"
libraryDependencies += "org.apache.lucene" % "lucene-core" % "4.10.4"

//libraryDependencies += "org.apache.lucene" % "lucene-analyzers-common" % "5.0.0"
libraryDependencies += "org.apache.lucene" % "lucene-analyzers-common" % "4.10.4"

// CP30
libraryDependencies += "c3p0" % "c3p0" % "0.9.0.4"

// JSOUP
libraryDependencies += "org.jsoup" % "jsoup" % "1.8.1"

// SCALA-CSV
resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.2.0"
	  
//yourDatabaseDependency is one of the supported databases :
	 
val posgresDriver = "postgresql" % "postgresql" % "8.4-701.jdbc4"

unmanagedBase := baseDirectory.value / "lib"

lazy val root = (project in file(".")).
	settings(
		name:= "StackOverflowViewer",
		version := "1.0",
		scalaVersion := "2.11.5"
		)

val buildSettings = Defaults.defaultSettings ++ Seq(
  javaOptions += "-Xmx8G"
)