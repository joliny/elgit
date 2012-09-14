define([
  'backbone',
  'views/home/main'
], function(Backbone, viewHomeMain) {
    var AppRouter = Backbone.Router.extend({
        routes: {
            '*actions': 'defaultAction'
        },

        defaultAction: function(actions) {
            viewHomeMain.render(); 
        }
    });

    var initialize = function() {
        var app_router = new AppRouter;
        Backbone.history.start();
    };

    return { 
        initialize: initialize
    };
});