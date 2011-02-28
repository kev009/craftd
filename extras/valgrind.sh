#!/bin/sh

if [ -d "src" ]; then
    case "$1" in
        thread)
            valgrind --tool=drd --exclusive-threshold=10 src/craftd -c craftd.conf.dist
        ;;

        *)
            valgrind -v --show-reachable=yes --leak-check=full --track-origins=yes src/craftd -c craftd.conf.dist
        ;;
    esac
else
  echo -e "\nRun from the project's repo root"
fi
