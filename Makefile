compile: src/*.hs
	cd src; make
	cp -f src/server ./

watch:
	inotifywait -mr -e close_write --exclude '\.o$$|\.hi$$|\.swp$$|\.swx$$|4913|server' src/ assets/ | while read; do make restart; done

start: compile
	mon ./server --attempts 1000 --pidfile server.pid

restart: compile
	kill `cat server.pid`

dist: compile
	strip -s server
	tar -caf dist.tar.gz server assets nginx.conf
