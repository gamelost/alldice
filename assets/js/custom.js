/* This works if the initial page load was the Docs page,
 * However this breaks if its not.
 *
 * Need to find a way to identify when the .bs-docs-sidebar and #sidebar shows up
 * to attach these events to it
 */
$(document).ready(function() {
    $('body').scrollspy({
        target: '.bs-docs-sidebar',
        offset: 40
    });
    $('#sidebar').affix({
        offset: {
            top: 60
        }
    });
});
