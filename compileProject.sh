mkdir -p ./compiled/

cp -f exercice2.ml ./compiled/exercice2.ml
cp -f exercice3.ml ./compiled/exercice3.ml
cp -f exercice4.ml ./compiled/exercice4.ml
cp -f exercice5.ml ./compiled/exercice5.ml
cp -f exercice6.ml ./compiled/exercice6.ml

cd ./compiled
ocamlc -g -o project.out exercice2.ml exercice3.ml exercice4.ml exercice5.ml exercice6.ml
cd ..
