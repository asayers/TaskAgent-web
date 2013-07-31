TaskAgent-web
=============

A simple to-do list web interface. Lists are saved to disk in the format
specified [here][1], so they can be synchronised with the TaskAgent [iPhone
app][4]. Uses [AngularJS][2] for the front-end and [Scotty][3] on the backend.
BSD 3-clause licenced.

To build the server binary, run `make`. You also need to write the URL of the
host into `hostname.conf`. Running the server will bring up the interface on
`localhost:3001`.

Check out the [demo][5] and the [docs][6].


[1]: http://macrecon.com/taskagent-formatting/
[2]: http://angularjs.org/
[3]: http://hackage.haskell.org/package/scotty
[4]: http://macrecon.com/app/TaskAgent/
[5]: http://todo.asayers.org/
[6]: http://asayers.github.io/TaskAgent-web/
