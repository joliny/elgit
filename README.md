# El Git
**Git repository browser in Erlang**

El Git is a git repository browser written in Erlang. It is based on
[Yaws](https://github.com/klacke/yaws) and
[Gert](https://github.com/mneudert/gert).


## Installation

Fetch the El Git repository and make it (fetches dependencies):

    $ git clone git://github.com/mneudert/elgit.git
    $ cd gert
    $ make

### Yaws configuration

El Git uses two yaws servers to dish out the content. One is the main server
sending repository data (http://elgit.dev/) and the other one is used for
static content (http://static.elgit.dev/).

Adding the following lines to your yaws.conf configures them:

    ebin_dir = /path/to/elgit/ebin
    ebin_dir = /path/to/elgit/ebin/gert/ebin

    runmod = elgit_app

    <server elgit.dev>
        port = 80
        listen = 0.0.0.0
        docroot = /path/to/elgit
        appmods = </, elgit_yaws>
    </server>

    <server static.elgit.dev>
        port = 80
        listen = 0.0.0.0
        docroot = /path/to/elgit/static
    </server>

And (re-)start Yaws.


## Usage

Open http://elgit.dev in your browser and enjoy "Hello World!".


## External Libraries/Sources

In alphabetical order:

- [Bootstrap](http://twitter.github.com/bootstrap/)
- [Gert](https://github.com/mneudert/gert/)
- [jQuery](http://jquery.com/)
- [libgit2](http://libgit2.github.com/)
- [Yaws](https://github.com/klacke/yaws/)


## Contributing

Fork El Git on GitHub, make it awesomer (preferably in a topic branch),
send a pull request.


## Authors

* Marc Neudert <marc.neudert@gmail.com>


## License

Currently looking for a suitable one. Will probably be one of
GPL (with linking exemption) or MIT.