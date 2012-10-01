define([
    'jquery',
    'underscore',
    'backbone'
], function($, _, Backbone) {
    var viewRepoTree = Backbone.View.extend({
        render: function(repo, oid, path) {
            var view = this;

            $('#tree .tree a').each(function(index) {
                $(this).click(function() {
                    view.changeTree(repo, oid, path, this);
                    return false;
                });
            });
        },

        changeTree: function(repo, oid, path, treeAnchor) {
            var view = this;
            var treeLink = '/' + repo +
                           '/tree/' + oid +
                           '/' + path + treeAnchor.text + '/';

            $.ajax({
                url: treeLink + '?partial=1',
                success: function(response) {
                    $('#repo').html(response);

                    Backbone.history.navigate(treeLink, true);
                    view.render(repo, oid, path + treeAnchor.text + '/');
                }
            });
        }
    });

    return new viewRepoTree;
});