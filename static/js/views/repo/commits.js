define([
    'jquery',
    'underscore',
    'backbone'
], function($, _, Backbone) {
    var viewRepoCommits = Backbone.View.extend({
        render: function(repo, oid) {
            // nothing to do here...
        }
    });

    return new viewRepoCommits;
});