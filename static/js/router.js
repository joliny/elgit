define([
  'backbone',
  'views/repo/main',
  'views/repo/tree'
], function(Backbone, viewRepoMain, viewRepoTree) {
    var AppRouter = Backbone.Router.extend({
        routes: {
            ':repo/tree/:oid/*path': 'repoTreeAction',
            ':repo/tree/:oid/': 'repoAction', // tree index
            ':repo/': 'repoAction',
            '*actions': 'defaultAction'
        },

        repoAction: function(repo) {
            viewRepoMain.render(repo);

            this.repoTreeAction(repo, 'master', '');
        },

        repoTreeAction: function(repo, oid, path) {
            viewRepoTree.render(repo, oid, path);
        },

        defaultAction: function(actions) {
            // nothing to do here...
        }
    });

    var initialize = function() {
        var app_router = new AppRouter;
        Backbone.history.start({pushState: true});
    };

    return {
        initialize: initialize
    };
});