#!/bin/bash -e
export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
rm -fR runtime slang cli mill-standalone versions.properties out sireum
curl -Lo sireum http://files.sireum.org/sireum
chmod +x sireum
git clone --depth 1 https://github.com/sireum/runtime
git clone --depth 1 https://github.com/sireum/slang
$SCRIPT_HOME/sireum slang tipe --verbose -r -s runtime/library:slang:shared
curl -Lo mill-standalone http://files.sireum.org/mill-standalone
chmod +x mill-standalone
curl -Lo versions.properties https://raw.githubusercontent.com/sireum/kekinian/master/versions.properties
$SCRIPT_HOME/mill-standalone alir.jvm.tests.test
