/* Awful override code for bootstrap and jquery support
 *
 * According to: http://www.sitepoint.com/understanding-bootstraps-affix-scrollspy-plugins/
 * We can use data-* attributes or javascript to handle this bit. So let's look into those
 */
(function (){
    [
        'http://0.0.0.0:8080/jquery.min.js',
        'http://0.0.0.0:8080/bootstrap.min.js'
    ].forEach(function(src) {
        var script = document.createElement('script');
        script.src = src;
        script.async = false;
        document.head.appendChild(script);
    });

    setInterval(
        function (){
            $('#sidebar').affix({
                offset: {
                    top: 60
                }
            });
            $('body').scrollspy({
                target: '.bs-docs-sidebar',
                offset: 40
            });
        }, 1000);
})();
