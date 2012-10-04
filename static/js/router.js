define([
  'backbone',
  'views/repo/main',
  'views/repo/commits',
  'views/repo/tree'
], function(Backbone, viewRepoMain, viewRepoCommits, viewRepoTree) {
    var AppRouter = Backbone.Router.extend({
        routes: {
            ':repo/commits/:oid/': 'repoCommitsAction',
            ':repo/tree/:oid/*path': 'repoTreeAction',
            ':repo/tree/:oid/': 'repoAction', // tree index
            ':repo/': 'repoAction',
            '*actions': 'defaultAction'
        },

        repoAction: function(repo) {
            this.repoTreeAction(repo, 'master', '');
        },

        repoCommitsAction: function(repo) {
            viewRepoMain.render(repo);
            viewRepoCommits.render(repo);
        },

        repoTreeAction: function(repo, oid, path) {
            viewRepoMain.render(repo);
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