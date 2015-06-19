module.exports = function(grunt) {
    "use strict";

    // http://stackoverflow.com/questions/23847250/grunt-with-grunt-sass-libsass-wrapper-slow-compile-time

    grunt.initConfig({
        pursSrcFiles: [
            "psrc/**/*.purs"
            , "bower_components/**/src/**/*.purs"
            , "bower_components/**/src/**/*.purs.hs"
        ],

        dotPsci: ["<%=pursSrcFiles%>"],

        psc: {
            options: {
                main: "Example.Main",
                modules: ["Example.Main"]
            },
            all: {
                src: [ "<%=pursSrcFiles%>" ],
                dest: "dist/example.js"
            }
        },
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
                    'dist/style.css': 'psrc/sass/style.scss'
                }
            }
        },

        copy: {
            all: {
                files: [
                    {
                        expand: true,
                        flatten: true,
                        src: [ 'psrc/**/*.html' ],
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
                            'bower_components/bootstrap-sass-official/assets/javascripts/*.js.map'
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
            purs: {
                files: [ "<%=pursSrcFiles%>" ],
                tasks: ["psc:all"]
            },
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
    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-contrib-copy");

    grunt.registerTask("default", ["psc:all", "dotPsci", "sass:all", "copy:all"]);
};
