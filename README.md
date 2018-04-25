# Scalastat
A Scala library for basic statistical applications

Scalastat is a purely functional statistics library written in Scala and based on complete immutability.
It strongly prioritises safety over performance and should not be used in time-critical production environments.

Scalastat is a one-man hobby project that is far from finished. Please do not expect a good usage experience or thorough documentation at this point.

Scalastat depends on [FunCSV](http://github.com/Seitzal/FunCSV) to read data from CSV files.
It has been tested with >50,000 observations and, while slow, has demonstrated capability to handle larger datasets.

Pull requests or bug reports are always appreciated.
I will also try to answer general questions / feature requests, but can't guarantee it, since this is a free-time project and I can't invest a lot of time in it.

# Get Scalastat
Pre-built jars can be downloaded from my [Jenkins](http://redundant.seitzal.org:8080).

To build it yourself, you need Git and [SBT](https://www.scala-sbt.org/).
1. Clone the repository:
  git clone https://github.com/seitzal/Scalastat
2. Compile the code:
  sbt compile
3. Build the package:
  sbt package
You can then find the .jar files in the target/scala-2.11 directory.
