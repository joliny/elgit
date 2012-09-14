define([
    'jquery',
    'underscore',
    'backbone',
    'text!/templates/home/main.html',
    'text!/templates/home/repo.html',
], function($, _, Backbone, templateHomeMain, templateHomeRepo) {
    var mainHomeView = Backbone.View.extend({
        el: $("#page"),

        render: function() {
            var compiledTemplate = _.template(templateHomeMain, {});

            $(this.el).html(compiledTemplate);

            var view = this;

            $.ajax({
                url: "/xhr/repo/init",
                dataType: 'json',
                success: function(repo) {
                    if (repo && repo.state && 'ok' == repo.state) {
                        view.renderRepo(repo.repo);
                    }
                }
            });
        },

        renderRepo: function(repo) {
            var data = {repoHeadSha: repo.HEAD.sha,
                        repoHeadMessage: repo.HEAD.message,
                        repoHeadAuthor: repo.HEAD.author,
                        repoHeadTimestamp: repo.HEAD.timestamp};
            var compiledTemplate = _.template(templateHomeRepo, data);

            $(this.el).html(compiledTemplate);
        }
    });

    return new mainHomeView;
});