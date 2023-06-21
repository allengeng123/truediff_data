


/home/allen/pythonparser/src/main/python/pythonparser

sbt -J-Xmx8G -Dgt.pp.path=/home/allen/pythonparser/src/main/python/pythonparser/pythonparser_3.py "project truediff_gumtree" "test:runMain truediff.compat.gumtree.PrepareBenchmarkPythonCommits"

## fetch keras data
truediff / test:runMain truediff.util.FetchCommits  

## check installed java version
eval System.getProperty("java.version")  


## run truediff on single test case
testOnly *MyOptionTests -- -z A  


## install sbt
//https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Linux.html\
curl -s "https://get.sdkman.io" | bash\
sdk install java $(sdk list java | grep -o "\b11\.[0-9]*\.[0-9]*\-tem" | head -1)\
sdk install sbt\

## url to artifacts
https://dl.acm.org/do/10.1145/3410286/full/

## clone python parser
git clone https://gitlab.rlp.net/plmz/external/pythonparser.git
