define([
  'backbone',
  'views/repo/main',
  'views/repo/commits',
  'views/repo/tags',
  'views/repo/tree'
], function(Backbone, viewRepoMain, viewRepoCommits, viewRepoTags, viewRepoTree) {
    var AppRouter = Backbone.Router.extend({
        routes: {
            ':repo/commits/:oid/': 'repoCommitsAction',
            ':repo/tags/': 'repoTagsAction',
            ':repo/tree/:oid/*path': 'repoTreeAction',
            ':repo/tree/:oid/': 'repoAction', // tree index
            ':repo/': 'repoAction',
            '*actions': 'defaultAction'
        },

        repoAction: function(repo, oid) {
            if (!oid) {
                oid = 'master';
            }

            this.repoTreeAction(repo, oid, '');
        },

        repoCommitsAction: function(repo, oid) {
            viewRepoMain.render(repo, oid);
            viewRepoCommits.render();
        },

        repoTagsAction: function(repo) {
            viewRepoMain.render(repo, 'master');
            viewRepoTags.render();
        },

        repoTreeAction: function(repo, oid, path) {
            viewRepoMain.render(repo, oid);
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