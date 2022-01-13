#!/bin/bash
export JAVA_HOME=$(/usr/libexec/java_home)
export PATH=$JAVA_HOME/bin:$PATH
source ~/.zshenv
echo $JAVA_HOME
echo $PATH
exec bash