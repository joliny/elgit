require.config({
    baseUrl: '/js',
    paths: {
        underscore: 'lib/lodash'
    },
    text: {
        useXhr: function (url, protocol, hostname, port) {
            //return true if you want to allow this url, given that the
            //text plugin thinks the request is coming from protocol,  hostname, port.
            return true;
        }
    }
});

require([], function() {
  require(['underscore'], function(_) {
        window._ = _;
    });

    require([
        'app',
        'order!jquery',
        'order!underscore',
        'order!backbone'
    ], function(App, $, _, Backbone) {
        App.initialize();
    });
});