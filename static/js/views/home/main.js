define([
    'jquery',
    'underscore',
    'backbone',
    'text!/templates/home/main.html',
    'text!/templates/home/repo.html',
    'text!/templates/home/tree.html',
], function($, _, Backbone, templateHomeMain, templateHomeRepo, templateHomeTree) {
    var mainHomeView = Backbone.View.extend({
        el: $("#page"),

        render: function() {
            var compiledTemplate = _.template(templateHomeMain, {});

            $(this.el).html(compiledTemplate);

            var view = this;

            $.ajax({
                url: "/xhr/repo/init",
                dataType: 'json',
                success: function(resp) {
                    if (resp && resp.state && 'ok' == resp.state) {
                        view.renderRepo(resp.repo);
                    }
                }
            });
        },

        renderRepo: function(repo) {
            var data = {repoHeadOid: repo.HEAD.oid,
                        repoHeadMessage: repo.HEAD.message,
                        repoHeadAuthor: repo.HEAD.author,
                        repoHeadTimestamp: repo.HEAD.timestamp};
            var compiledTemplate = _.template(templateHomeRepo, data);

            $(this.el).html(compiledTemplate);

            var view = this;

            $.ajax({
                url: "/xhr/repo/tree/" + repo.HEAD.oid + "/",
                dataType: 'json',
                success: function(resp) {
                    if (resp && resp.state && 'ok' == resp.state) {
                        view.renderTree(resp.tree);
                    }
                }
            });
        },

        renderTree: function(tree) {
            var data = {entries: tree.entries};
            var compiledTemplate = _.template(templateHomeTree, data);

            $(this.el).append(compiledTemplate);
        }
    });

    return new mainHomeView;
});