define([
    'jquery',
    'underscore',
    'backbone',
    'views/repo/commits',
    'views/repo/tree'
], function($, _, Backbone, viewRepoCommits, viewRepoTree) {
    var viewRepoMain = Backbone.View.extend({
        render: function(repo) {
            var view = this;

            $('#repo-head .nav-tabs a').each(function(index) {
                var actionHref = $(this).attr('href');

                $(this).click(function() {
                    $('#repo-head .nav-tabs li').each(function(index) {
                        $(this).removeClass('active');
                    });

                    $(this).parent().addClass('active');

                    view.changeAction(actionHref);
                    return false;
                });
            });
        },

        changeAction: function(actionHref) {
            actionParts = actionHref.match(/^\/(.+?)\/(.+?)\/(.+?)\/$/);

            $.ajax({
                url: actionHref + '?partial=1',
                success: function(response) {
                    $('#repo').html(response);

                    Backbone.history.navigate(actionHref, true);

                    if ('commits' == actionParts[2]) {
                        viewRepoCommits.render(actionParts[1]);
                    } else if ('tree' == actionParts[2]) {
                        viewRepoTree.render(actionParts[1], actionParts[3], '');
                    }
                }
            });
        }
    });

    return new viewRepoMain;
});