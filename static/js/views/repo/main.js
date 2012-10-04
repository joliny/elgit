define([
    'jquery',
    'underscore',
    'backbone',
    'views/repo/commits',
    'views/repo/tags',
    'views/repo/tree'
], function($, _, Backbone, viewRepoCommits, viewRepoTags, viewRepoTree) {
    var viewRepoMain = Backbone.View.extend({
        render: function(repo, oid) {
            var view = this;

            $('#repo-head .nav-tabs a').each(function(index) {
                $(this).click(function() {
                    $('#repo-head .nav-tabs li').each(function(index) {
                        $(this).removeClass('active');
                    });

                    $(this).parent().addClass('active');

                    view.changeAction($(this).attr('href'));
                    return false;
                });
            });

            $('#tree-branch').change(function() {
                var branch = $(this).val();

                if (!branch) {
                    return;
                }

                $('#repo-head .nav-tabs a').each(function(index) {
                    /**
                     * actionParts @1 = repo
                     * actionParts @2 = action
                     * actionParts @4 = branch
                     */
                    var actionHref  = $(this).attr('href');
                    var actionParts = actionHref.match(/^\/(.+?)\/(.+?)(\/(.+?))?\/$/);

                    if (!!actionParts[4] && branch != actionParts[4]) {
                        actionHref = '/' + actionParts[1] +
                                     '/' + actionParts[2] +
                                     '/' + branch + '/';

                        $(this).attr('href', actionHref);
                    }
                });

                $('#repo-head .nav-tabs li.active a').click();
            });
        },

        changeAction: function(actionHref) {
            /**
             * actionParts @1 = repo
             * actionParts @2 = action
             * actionParts @4 = branch
             */
            var actionParts = actionHref.match(/^\/(.+?)\/(.+?)(\/(.+?))?\/$/);

            $.ajax({
                url: actionHref + '?partial=1',
                success: function(response) {
                    $('#repo').html(response);

                    Backbone.history.navigate(actionHref, true);

                    if ('commits' == actionParts[2]) {
                        viewRepoCommits.render();
                    } else if ('tags' == actionParts[2]) {
                        viewRepoTags.render();
                    } else if ('tree' == actionParts[2]) {
                        viewRepoTree.render(actionParts[1], actionParts[4], '');
                    }
                }
            });
        }
    });

    return new viewRepoMain;
});