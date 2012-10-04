define([
    'jquery',
    'underscore',
    'backbone'
], function($, _, Backbone) {
    var viewRepoTree = Backbone.View.extend({
        render: function(repo, oid, path) {
            var view = this;

            $('#tree-branch').change(function() {
                var branch = $(this).val();

                if (!branch) {
                    return;
                }

                view.changeTree(repo, branch, '');
            });

            $('#tree-crumb a').each(function(index) {
                treeParts = $(this).attr('href').match(/^\/.+\/tree\/.+?\/(.*)$/);

                $(this).click(function() {
                    view.changeTree(repo, oid, treeParts[1]);
                    return false;
                });
            });

            $('#tree .tree a').each(function(index) {
                $(this).click(function() {
                    view.changeTree(repo, oid, path + this.text + '/');
                    return false;
                });
            });
        },

        changeTree: function(repo, oid, path) {
            var view     = this;
            var treeLink = '/' + repo +
                           '/tree/' + oid +
                           '/' + path;

            $.ajax({
                url: treeLink + '?partial=1',
                success: function(response) {
                    $('#repo').html(response);

                    Backbone.history.navigate(treeLink, true);
                    view.render(repo, oid, path);
                }
            });
        }
    });

    return new viewRepoTree;
});