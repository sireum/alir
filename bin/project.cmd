::#! 2> /dev/null                                   #
@ 2>/dev/null # 2>nul & echo off & goto BOF         #
if [ -z ${SIREUM_HOME} ]; then                      #
  echo "Please set SIREUM_HOME env var"             #
  exit -1                                           #
fi                                                  #
exec ${SIREUM_HOME}/bin/sireum slang run "$0" "$@"  #
:BOF
setlocal
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
%SIREUM_HOME%\bin\sireum.bat slang run "%0" %*
exit /B %errorlevel%
::!#
// #Sireum

import org.sireum._
import org.sireum.project.ProjectUtil._
import org.sireum.project.Project

val tipe = "slang-tipe"

val alir = "alir"

val homeDir = Os.slashDir.up.canon

val alirShared = moduleSharedPub(
  id = alir,
  baseDir = homeDir,
  sharedDeps = ISZ(tipe),
  sharedIvyDeps = ISZ(),
  pubOpt = pub(
    desc = "Alir Flow Framework",
    url = "github.com/sireum/alir",
    licenses = bsd2,
    devs = ISZ(robby)
  )
)

val project = Project.empty + alirShared

projectCli(Os.cliArgs, project)
