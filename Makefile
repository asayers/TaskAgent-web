compile: src/*.hs
	cd src; make
	cp -f src/server ./

start: compile
	mon ./server --attempts 1000 --pidfile server.pid

restart: compile
	kill `cat server.pid`
