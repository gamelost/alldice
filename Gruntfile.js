module.exports = function(grunt) {
    "use strict";

    // http://stackoverflow.com/questions/23847250/grunt-with-grunt-sass-libsass-wrapper-slow-compile-time
    grunt.initConfig({
        sass: {
            options: {
                precision: 10,
                sourceMap: true,
                sourceComments: false,
                outputStyle: 'expanded',
                includePaths: [
                    "node_modules/bootstrap-sass/assets/stylesheets/"
                ]
            },
            all: {
                files: {
                    'dist/style.css': 'assets/sass/style.scss'
                }
            }
        },

        copy: {
            all: {
                files: [
                    {
                        expand: true,
                        flatten: true,
                        src: [ 'assets/**/*.html' ],
                        dest: 'dist/',
                        filter: 'isFile'
                    },
                    {
                        expand: true,
                        flatten: true,
                        src: [
                            'node_modules/jquery/dist/*.js',
                            'node_modules/jquery/dist/*.js.map',
                            'node_modules/bootstrap-sass/assets/javascripts/*.js',
                            'node_modules/bootstrap-sass/assets/javascripts/*.js.map',
                        ],
                        dest: 'dist/',
                        filter: 'isFile'
                    }
                ]
            }
        }
    });

    grunt.loadNpmTasks('grunt-sass');
    grunt.loadNpmTasks("grunt-contrib-copy");

    grunt.registerTask("default", ["sass:all", "copy:all"]);
};
