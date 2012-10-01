define([
    'jquery',
    'underscore',
    'backbone'
], function($, _, Backbone) {
    var viewRepoMain = Backbone.View.extend({
        render: function(repo) {
            // nothing to do here...
        }
    });

    return new viewRepoMain;
});