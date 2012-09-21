define([
    'jquery',
    'underscore',
    'backbone',
    'text!/templates/home/main.html',
    'views/home/repo'
], function($, _, Backbone, templateHomeMain, viewHomeRepo) {
    var viewHomeMain = Backbone.View.extend({
        el: "#page",

        render: function() {
            var compiledTemplate = _.template(templateHomeMain, {});

            $(this.el).html(compiledTemplate);

            viewHomeRepo.render();
        }
    });

    return new viewHomeMain;
});