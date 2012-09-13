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

Add the following lines to your yaws.conf:

    ebin_dir = /path/to/elgit/ebin
    ebin_dir = /path/to/elgit/ebin/gert/ebin

    runmod = elgit_app

    <server elgit.dev>
        port = 80
        listen = 0.0.0.0
        docroot = /path/to/elgit
        appmods = </, elgit_yaws>
    </server>

And (re-)start Yaws.


## Usage

Open http://elgit.dev in your browser and enjoy "Hello World!".


## Contributing

Fork El Git on GitHub, make it awesomer (preferably in a topic branch),
send a pull request.


## Authors

* Marc Neudert <marc.neudert@gmail.com>


## License

Currently looking for a suitable one. Will probably be one of
GPL (with linking exemption) or MIT.