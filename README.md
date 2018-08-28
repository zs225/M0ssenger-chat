# Final-Project

## System Configuration

IMPORTANT NOTE:
Many external packages must be installed first, including
- Yojson
- Cohttp
- Cohttp-lwt-unix
- Cohttp-lwt
- Cohttp-top
- lwt_ssl
- ssl
- tls
- Cohttp-async
- lwt
- ANSITerminal
- Emoji
- mysql-server
- libmysqlclient-dev
- mysql

We've streamlined this process for your convenience by putting it all in a
script.  Therefore, if it is the first time you are using our system, cd
into the project folder and type:

```make all```

in your terminal.

Note:  If you do not have mysql, you will be prompted to enter a "root" password
during the installation. Make this whatever you want.  Later on, you will be
prompted for the root password, so make sure you remember it.  Immediately
after, you will be prompted with an additional password.  This password is
simply "a".

After this, your system should be configured to run our app.

To actually run the app, you need to open a separate terminal session to run
your server and every instance of the app you desire.

To run the server, type ```make server```.  This is set to port 8080 by default.

To run an instance of the app, type ```make app```.  This will send requests
to port 8080 by default.


Those commands are likely the ones you are most concerned with, but we have
other "make" commands if you are interested:

```make buildsystem``` compiles everything


```make group-test``` runs an interactive test group chat that continually pulls
from the server.  Command line arguments can be used to send messages and see
if they are geetting through.

```make db``` compiles the database

```make init-db``` reset tables in the database

```make create-db``` creates the database in mysql

```make clean``` cleans up products of compilation

```make install``` installs library dependencies


## Using M0ssenger

We hope this app is beginner-friendly (use :help whenever you need it); here
are a few tips to get you started:

- Since this is a CLI app, the user manipulates the state with the help of
typed out commands.  These commands follow the format :examplecommand.
You can type :help for a list of these and a description of each.  Do note that
if you plan on typing a command (as opposed to, say, a message to another
person), this must be the only thing you type on the line.  The command will
not be recognized if you embed it within a longer string.

- Our CLI supports emojis so you can add some personality to your texts.
The format for typing an emoji is :example_emoji:.  You can type :emojis for
a list of emojis the app supports and the input necessary to produce them.  You
can even include an emoji(s) in your name!  However, some may look goofy upon
rendering in the CLI





