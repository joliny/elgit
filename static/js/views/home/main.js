define([
    'jquery',
    'underscore',
    'backbone',
    'text!/templates/home/main.html'
], function($, _, Backbone, templateHomeMain) {
    var mainHomeView = Backbone.View.extend({
        el: $("#page"),
        render: function() {
            var data = {};
            var compiledTemplate = _.template(templateHomeMain, data);

            $(this.el).append(compiledTemplate);
        }
    });

    return new mainHomeView;
});