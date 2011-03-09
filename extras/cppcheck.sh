#!/bin/sh

if [ -d "src" ]; then
  cppcheck -I include -v --force --enable=all src/ plugins/
else
  echo -e "\nRun from the project's repo root"
fi
