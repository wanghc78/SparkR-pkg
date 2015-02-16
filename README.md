# R on Spark

[![Build Status](https://travis-ci.org/amplab-extras/SparkR-pkg.png?branch=master)](https://travis-ci.org/amplab-extras/SparkR-pkg)

SparkR is an R package that provides a light-weight frontend to use Spark from
R.


## Installing SparkR

### Requirements
SparkR requires Scala 2.10 and Spark version >= 0.9.0. Current build by default uses
Apache Spark 1.1.0. You can also build SparkR against a
different Spark version (>= 0.9.0) by modifying `pkg/src/build.sbt`.

SparkR also requires the R package `rJava` to be installed. To install `rJava`,
you can run the following command in R:

    install.packages("rJava")


### Package installation
To develop SparkR, you can build the scala package and the R package using

    ./install-dev.sh

If you wish to try out the package directly from github, you can use [`install_github`](http://www.inside-r.org/packages/cran/devtools/docs/install_github) from [`devtools`](http://www.inside-r.org/packages/cran/devtools). Note that you can specify which branch, tag etc to install from.

    library(devtools)
    install_github("amplab-extras/SparkR-pkg", subdir="pkg")

SparkR by default uses Apache Spark 1.1.0. You can switch to a different Spark
version by setting the environment variable `SPARK_VERSION`. For example, to
use Apache Spark 1.2.0, you can run

    SPARK_VERSION=1.2.0 ./install-dev.sh

SparkR by default links to Hadoop 1.0.4. To use SparkR with other Hadoop
versions, you will need to rebuild SparkR with the same version that [Spark is
linked
to](http://spark.apache.org/docs/latest/index.html#a-note-about-hadoop-versions).
For example to use SparkR with a CDH 4.2.0 MR1 cluster, you can run

    SPARK_HADOOP_VERSION=2.0.0-mr1-cdh4.2.0 ./install-dev.sh

By default, SparkR uses [sbt](http://www.scala-sbt.org) to build an assembly
jar. If you wish to use [maven](http://maven.apache.org/) instead, you can set
the environment variable `USE_MAVEN=1`. For example

    USE_MAVEN=1 ./install-dev.sh
    
If you are building SparkR from behind a proxy, you can [setup maven](https://maven.apache.org/guides/mini/guide-proxies.html) to use the right proxy
server.


## Running sparkR
If you have cloned and built SparkR, you can start using it by launching the SparkR
shell with

    ./sparkR

The `sparkR` script automatically creates a SparkContext with Spark by default in
local mode. To specify the Spark master of a cluster for the automatically created
SparkContext, you can run

    MASTER=<Spark master URL> ./sparkR
    
If you have installed it directly from github, you can include the SparkR
package and then initialize a SparkContext. For example to run with a local
Spark master you can launch R and then run

    library(SparkR)
    sc <- sparkR.init(master="local")

To increase the memory used by the driver you can export the SPARK\_MEM
environment variable. For example to use 1g, you can run

    SPARK_MEM=1g ./sparkR

In a cluster setting to set the amount of memory used by the executors you can
pass the variable `spark.executor.memory` to the SparkContext constructor.

    library(SparkR)
    sc <- sparkR.init(master="spark://<master>:7077",
                      sparkEnvir=list(spark.executor.memory="1g"))

Finally, to stop the cluster run

    sparkR.stop()
    
sparkR.stop() can be invoked to terminate a SparkContext created previously via sparkR.init(). Then you can call sparR.init() again to create a new SparkContext that may have different configurations.
    
## Examples, Unit tests

SparkR comes with several sample programs in the `examples` directory.
To run one of them, use `./sparkR <filename> <args>`. For example:

    ./sparkR examples/pi.R local[2]

You can also run the unit-tests for SparkR by running

    ./run-tests.sh

## Running on EC2

Instructions for running SparkR on EC2 can be found in the
[SparkR wiki](https://github.com/amplab-extras/SparkR-pkg/wiki/SparkR-on-EC2).

## Running on YARN
Currently, SparkR supports running on YARN with the `yarn-client` mode. These steps show how to build SparkR with YARN support and run SparkR programs on a YARN cluster:

```
# assumes Java, R, rJava, yarn, spark etc. are installed on the whole cluster.
cd SparkR-pkg/
USE_YARN=1 SPARK_YARN_VERSION=2.4.0 SPARK_HADOOP_VERSION=2.4.0 ./install-dev.sh
```

Before launching an application, make sure each worker node has a local copy of `lib/SparkR/sparkr-assembly-0.1.jar`. With a cluster launched with the `spark-ec2` script, do:
```
~/spark-ec2/copy-dir ~/SparkR-pkg
```

Finally, when launching an application, the environment variable `YARN_CONF_DIR` needs to be set to the directory which contains the client-side configuration files for the Hadoop cluster (with a cluster launched with `spark-ec2`, this defaults to `/root/ephemeral-hdfs/conf/`):
```
YARN_CONF_DIR=/root/ephemeral-hdfs/conf/ MASTER=yarn-client ./sparkR
YARN_CONF_DIR=/root/ephemeral-hdfs/conf/ ./sparkR examples/pi.R yarn-client
```

## Report Issues/Feedback 

For better tracking and collaboration, issues and TODO items are reported to a dedicated [SparkR JIRA](https://sparkr.atlassian.net/browse/SPARKR/).

In your pull request, please cross reference the ticket item created. Likewise, if you already have a pull request ready, please reference it in your ticket item.
