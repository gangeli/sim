# -- VARIABLES --
# (places -- overwrite me!)
SCALA_HOME=${HOME}/programs/scala
# (programs)
JAVAC=javac
SCALAC=${SCALA_HOME}/bin/scalac
SCALA=${SCALA_HOME}/bin/scala
SCALADOC=${SCALA_HOME}/bin/scaladoc
# (locations)
SRC=src
TEST_SRC=test/src
LIB=etc
BUILD=bin
TEST_BUILD=test/bin
DIST=dist
TMP=tmp
DOC=scaladoc
# (classpaths)
CORENLP=${LIB}/stanford-corenlp-1.3.5.jar
CP=${CORENLP}:${LIB}/jaws.jar:${LIB}/breeze-math.jar:${LIB}/json.jar

# -- BUILD --
${DIST}/sim.jar: $(wildcard src/org/goobs/sim/*.scala) $(wildcard src/org/goobs/sim/viz/*.scala) $(wildcard src/edu/stanford/nlp/*.scala)
	@mkdir -p ${BUILD}
	@echo "Compiling (${JAVAC})..."
	@${JAVAC} -deprecation -d ${BUILD} -cp ${CP} `find ${SRC} -name "*.java"`
	@echo "Compiling (${SCALAC})..."
	@${SCALAC} -deprecation -d ${BUILD} -cp ${CP} `find ${SRC} -name "*.scala"` `find ${SRC} -name "*.java"`
	mkdir -p ${DIST}
	jar cf ${DIST}/sim.jar -C $(BUILD) .
	jar uf ${DIST}/sim.jar -C $(SRC) .

${DIST}/sim-release.jar: ${DIST}/sim.jar
	mkdir -p ${TMP}
	cp ${DIST}/sim.jar ${DIST}/sim-release.jar
	#((scala-library))
	rm -rf ${TMP}/scala-library
	unzip ${SCALA_HOME}/lib/scala-library.jar -d ${TMP}/scala-library > /dev/null
	rm -r ${TMP}/scala-library/META-INF
	jar uf ${DIST}/sim-release.jar -C ${TMP}/scala-library/ .
	rm -rf ${TMP}/scala-library
	#((scala-compiler))
	rm -rf ${TMP}/scala-compiler
	unzip ${SCALA_HOME}/lib/scala-compiler.jar -d ${TMP}/scala-compiler > /dev/null
	rm -r ${TMP}/scala-compiler/META-INF
	jar uf ${DIST}/sim-release.jar -C ${TMP}/scala-compiler/ .
	rm -rf ${TMP}/scala-compiler
	#((jaws))
	rm -rf ${TMP}/jaws
	unzip ${LIB}/jaws.jar -d ${TMP}/jaws > /dev/null
	rm -r ${TMP}/jaws/META-INF
	jar uf ${DIST}/sim-release.jar -C ${TMP}/jaws/ .
	rm -rf ${TMP}/jaws
	#((json))
	rm -rf ${TMP}/json
	unzip ${LIB}/json.jar -d ${TMP}/json > /dev/null
	rm -r ${TMP}/json/META-INF
	jar uf ${DIST}/sim-release.jar -C ${TMP}/json/ .
	rm -rf ${TMP}/json
	#((breeze-math))
	rm -rf ${TMP}/breeze-math
	unzip ${LIB}/breeze-math.jar -d ${TMP}/breeze-math > /dev/null
	rm -r ${TMP}/breeze-math/META-INF
	jar uf ${DIST}/sim-release.jar -C ${TMP}/breeze-math/ .
	rm -rf ${TMP}/breeze-math
	#((stanford-corenlp-1.3.5.jar))
	rm -rf ${TMP}/stanford-corenlp-1.3.5.jar
	unzip ${LIB}/stanford-corenlp-1.3.5.jar -d ${TMP}/stanford-corenlp-1.3.5.jar > /dev/null
	rm -r ${TMP}/stanford-corenlp-1.3.5.jar/META-INF
	jar uf ${DIST}/sim-release.jar -C ${TMP}/stanford-corenlp-1.3.5.jar/ .
	rm -rf ${TMP}/stanford-corenlp-1.3.5.jar

doc:
	@echo "Documenting (${SCALADOC})..."
	mkdir -p ${DOC}
	@${SCALADOC} -d ${DOC} -cp ${CP} `find ${SRC} -name "*.scala"` `find ${SRC} -name "*.java"`

# -- TARGETS --
default: ${DIST}/sim.jar

release: ${DIST}/sim-release.jar

clean:
	rm -rf ${BUILD}
	rm -f ${DIST}/sim.jar
	rm -rf ${TMP}
	rm -f java.hprof.txt
