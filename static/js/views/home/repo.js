define([
    'jquery',
    'underscore',
    'backbone',
    'text!/templates/home/repo.html',
    'views/home/tree',
], function($, _, Backbone, templateHomeRepo, viewHomeTree) {
    var viewHomeRepo = Backbone.View.extend({
        render: function() {
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

            $("#repo").html(compiledTemplate);

            viewHomeTree.render(repo);
        }
    });

    return new viewHomeRepo;
});