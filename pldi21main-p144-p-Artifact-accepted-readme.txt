# Getting Started Guide

The artifact is provided as a Docker container. 
Before building the Docker container set the memory of Docker containers to at least 8GB.
Follow the tutorials for MacOs or Windows:
    - https://docs.docker.com/docker-for-mac/#memory
    - https://docs.docker.com/docker-for-windows/#advanced

The docker container can be built with the following command within the artifact directory: `docker build --network=host -t truediff-artifact .`

The Dockerfile will clone the repository of the open-source artifact (https://gitlab.rlp.net/plmz/truediff/-/tree/pldi21-artifact), whose code is also included in the artifact itself. The Dockerfile will also clone the Python parser used by Gumtree to execute the benchmark detailed in Secion 6. Further it will built the sbt project and execute its tests.


# Step-by-Step Instructions

## Section 1 and 2
- The examples discussed during the Introduction (line 75ff in the paper) and in Section 2 (line 244ff in the paper) are present in `truediff/truediff/src/test/scala/truediff/macros/ExpTests.scala` line 162ff.

## Section 3
- The abstract syntax of truechange edit scripts (Figure 1) are present in the package located at `truediff/truechange/src/main/scala/truechange`.
- The example of succesive changes to a program (line 337ff) are shown in `truediff/truediff/src/test/scala/truediff/macros/ExpTests.scala` line 177ff.
- The standard semantics presented in Figure 2 of the paper is taken from `truediff/truechange/src/main/scala/truechange/StandardSemantics.scala`.
- The type system of the edit script language shown in Figure 3 of the paper is implemented as the method `well-typed` in `truediff/truechange/src/main/scala/truechange/EditScripts.scala`.

## Section 4
- The `Diffable` trait and its methods that are discussed during Section 4 are implemented in `truediff/truediff/src/main/scala/truediff/Diffable.scala`. Note that the method `assignSharesRec` is called `assignSharesRecurse`, `computeEdits` is called `computeEditScript`, `updateLits` is called `updateLiterals`, and `computeEditsRec` is called `computeEditScriptRecurse` in the implementation. 

## Section 5
- The usage of our `diffable` macro shown in line 1050ff in the paper is taken from `truediff/truediff/src/test/scala/truediff/macros/Exp.scala` of the repository.
- The ANTLR wrapper is implemented within the class `truediff/truediff-antlr/src/main/scala/truediff/compat/antlr/DiffableRuleContext.scala` with executable examples in `truediff/truediff-antlr/src/test/scala/truediff/compat/antlr/java/TestHelloWorld.scala`.
- The Gumtree wrapper is implemented within the class `truediff/truediff-gumtree/src/main/scala/truediff/compat/gumtree/DiffableGumTree.scala`.

## Section 6
The measurements were peformed on an Intel Core i7 at 2.7GHz with 16 GB of RAM, running 64-bit OSX 10.15.7, Java1.11.0.5 with 8GB max heap space and they were not performed in a Docker container.

To run the measurements and shown in the evaluation you need to follow these steps:
- Run the previously built Docker container by running the following command `docker run --network=host -it truediff-artifact /bin/bash`

- Prepare the Python project that was used to perform the evaluation by executing the following commands in the running Docker container (can take up to an hour):
    - `sbt "project truediff" "test:runMain truediff.util.FetchCommits"`
    - `sbt -J-Xmx8G -Dgt.pp.path=/home/worker/pythonparser/src/main/python/pythonparser/pythonparser_3.py "project truediff_gumtree" "test:runMain truediff.compat.gumtree.PrepareBenchmarkPythonCommits"`

- Perform the measurements for gumtree by executing the following command:
    - `sbt -J-Xmx8G "project truediff_gumtree" "test:runMain truediff.compat.gumtree.BenchmarkPythonCommitsGumtree"`
- Perform the measurements for the truediff algorithm on gumtree trees by executing the following command:
    - `sbt -J-Xmx8G "project truediff_gumtree" "test:runMain truediff.compat.gumtree.BenchmarkPythonCommitsTruediffGumtree"`
- `sbt "project truediff" "test:runMain truediff.util.FetchCommits"`

- These commands will save their measurements in `benchmark/measurements/python_keras_500_measurements-gumtree.csv` and `benchmark/measurements/python_keras_500_measurements-truediffGumtree.csv`.
- Rename these two files two keep the measurements presented in the paper.

Due to the fact that `hdiff` is a Haskell project and its dependencies it was not possible for us to achieve reproducibility of the `hdiff` measurements.
However, we provide the measurements in the file `benchmark/measurements/python_keras_500_measurements-hdiff.csv` and the accompanying xlsx file.

- Create two xlsx files that have the same names as the previously generated csv files. 
- Import the csv files into the xlsx files respectively while choosing the following options: 
    - check delimited and start import at Row 1
    - check comma as separator
    - click the advanced button
        - choose decimal separator `.`
        - choose thousands separator `` (no separator)
- After importing the two csv files into xlsx files, open the file `python_keras_500_measurements-comparison.xlsx` and select the update button.
- This action will update the files import the values of the other xlsx files and update the graphs accordingly.
- This file also contains the two graphs we showed in Section 6


## Section 7
- The usage of the `truechange` edit script language as a driver for incremental static analysis framework IncA is shown in the methods `processEditScript` and `processEdit` in `https://gitlab.rlp.net/plmz/inca-scala/-/blob/0cb9c74f618c8b556f7a309e45e895df3329a3d5/src/main/scala/inca/runtime/Database.scala#L112`.
