#!/bin/sh

if [ "src" ]; then
    case "$1" in
        thread)
            valgrind --log-file="valgrind-`date +%s`.log" --tool=drd --exclusive-threshold=100 ./craftd -c craftd.conf.dist
        ;;

        call)
            valgrind --log-file="valgrind-`date +%s`.log" --tool=callgrind ./craftd -c craftd.conf.dist
            echo -e "\nUse a tool like kcachegrind for visualization of the .out file"
        ;;

        *)
            valgrind --log-file="valgrind-`date +%s`.log" -v --show-reachable=yes --leak-check=full --track-origins=yes ./craftd -c craftd.conf.dist
        ;;
    esac
else
  echo -e "\nRun from the project's repo root"
fi
