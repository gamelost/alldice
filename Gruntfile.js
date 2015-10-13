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
                    "bower_components/bootstrap-sass-official/assets/stylesheets/"
                ]
            },
            all: {
                files: {
                    'dist/style.css': 'elm/sass/style.scss'
                }
            }
        },

        copy: {
            all: {
                files: [
                    {
                        expand: true,
                        flatten: true,
                        src: [ 'elm/**/*.html' ],
                        dest: 'dist/',
                        filter: 'isFile'
                    },
                    {
                        expand: true,
                        flatten: true,
                        src: [
                            'bower_components/jquery/dist/*.js',
                            'bower_components/jquery/dist/*.js.map',
                            'bower_components/bootstrap-sass-official/assets/javascripts/*.js',
                            'bower_components/bootstrap-sass-official/assets/javascripts/*.js.map',
                            'bower_components/react/*.js'
                        ],
                        dest: 'dist/',
                        filter: 'isFile'
                    }
                ]
            }
        },

        clean : {
            src: [ 'dist' ]
        },

        watch: {
            css: {
                files: [ "psrc/**/*.scss" ],
                tasks: ["sass:all"]
            },
            html: {
                files: [ "psrc/**/*.html" ],
                tasks: ["copy:all"]
            }
        },
    });

    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-sass');
    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-contrib-copy");

    grunt.registerTask("default", ["sass:all", "copy:all"]);
};
