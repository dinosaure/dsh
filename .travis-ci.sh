echo "yes" | sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install ocaml ocaml-native-compilers camlp4-extra opam opam

export OPAMYES=1
opam init
opam install oasis ocamlfind menhir ounit batteries
eval `opam config env`

./configure --enable-tests
make
make test
