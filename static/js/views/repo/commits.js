define([
    'jquery',
    'underscore',
    'backbone'
], function($, _, Backbone) {
    var viewRepoCommits = Backbone.View.extend({
        render: function(repo) {
            // nothing to do here...
        }
    });

    return new viewRepoCommits;
});