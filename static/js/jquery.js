define([
    // Load the original jQuery source file
    'order!lib/jquery'
], function() {
    // Tell Require.js that this module returns a reference to jQuery
    return jQuery;
});