#!/bin/sh

if [ -d "src" ]; then
    valgrind -v --show-reachable=yes --leak-check=full --track-origins=yes src/craftd -c craftd.conf.dist
else
  echo -e "\nRun from the project's repo root"
fi
