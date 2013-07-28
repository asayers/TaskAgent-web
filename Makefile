compile: src/*.hs
	(cd src && ghc -o server -O2 Server.hs)
	mv src/server ./
	strip -s server

start: compile
	mon ./server --attempts 1000 --pidfile server.pid

restart: compile
	kill `cat server.pid`
