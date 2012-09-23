define([
    'jquery',
    'underscore',
    'backbone',
    'text!/templates/home/tree.html',
], function($, _, Backbone, templateHomeTree) {
    var viewHomeTree = Backbone.View.extend({
        render: function(repo) {
            var view = this;

            $.ajax({
                url: "/xhr/repo/" + repo.id + "/tree/" + repo.HEAD.oid + "/",
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

            $("#tree").html(compiledTemplate);
        }
    });

    return new viewHomeTree;
});