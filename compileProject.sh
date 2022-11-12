mkdir -p ./compiled/

ocamlc -g -o project.out exercice2.ml exercice3.ml exercice4.ml exercice5.ml exercice6.ml

mv -f exercice2.cmo ./compiled/exercice2.cmo
mv -f exercice3.cmo ./compiled/exercice3.cmo
mv -f exercice4.cmo ./compiled/exercice4.cmo
mv -f exercice5.cmo ./compiled/exercice5.cmo
mv -f exercice6.cmo ./compiled/exercice6.cmo

mv -f exercice2.cmi ./compiled/exercice2.cmi
mv -f exercice3.cmi ./compiled/exercice3.cmi
mv -f exercice4.cmi ./compiled/exercice4.cmi
mv -f exercice5.cmi ./compiled/exercice5.cmi
mv -f exercice6.cmi ./compiled/exercice6.cmi
