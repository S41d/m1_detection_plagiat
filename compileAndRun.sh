#! /bin/bash

mkdir -p ./compiled/

filename="${1%.*}"
extension="${1##*.}"

if [ "$filename" == "" ];
then
    echo "please add filename"
    return 1;
elif [ $extension != "ml" ];
then
    echo "please add a ml file"
    return 1;
fi

cp $1 ./compiled/$1
cd ./compiled
ocamlc unix.cma -o $filename.byt $1
rm ./$1
cd ..

if [ "$2" != "" ];
then
    ./compiled/$filename.byt $2
else 
    ./compiled/$filename.byt
fi