buildsystem:
	ocamlbuild -use-ocamlfind -pkg cohttp-lwt-unix app.byte &&  \
	ocamlbuild -use-ocamlfind -pkg cohttp-lwt-unix start_server.byte

app:
	ocamlbuild -use-ocamlfind -pkg cohttp-lwt-unix start_app.byte && \
	./start_app.byte

server:
	ocamlbuild -use-ocamlfind -pkg cohttp-lwt-unix start_server.byte && \
	./start_server.byte


group-test:
	ocamlbuild -use-ocamlfind test_group_chat.byte &&\
	ocamlbuild -use-ocamlfind test_group_chat_1.byte &&\
	ocamlbuild -use-ocamlfind test_group_chat_2.byte &&\
	ocamlbuild -use-ocamlfind test_group_chat_3.byte &&\
	./test_group_chat.byte

db:
	ocamlbuild -use-ocamlfind data_base.byte &&	./data_base.byte

init-db , reset-db:
	ocamlbuild -use-ocamlfind init.byte &&	./init.byte

create-db:
	mysql -u root -p < newuser.sql && mysql -u newuser -p < create_db.sql

clean:
	ocamlbuild -clean

install:
	opam install yojson ANSITerminal cohttp lwt cohttp-lwt-unix cohttp-lwt \
	cohttp-top cohttp-async ssl lwt_ssl emoji && \
 	sudo apt-get update && sudo apt-get install mysql-server && \
 	sudo apt-get install libmysqlclient-dev && opam install mysql

all: install create-db init-db buildsystem
