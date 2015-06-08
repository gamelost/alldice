module.exports = function(grunt) {
  "use strict";

  grunt.initConfig({

    psc: {
      options: {
        main: "Example.Main",
        modules: ["Example.Main"]
      },
      all: {
        src: [ "psrc/**/*.purs"
             , "bower_components/**/src/**/*.purs"],
        dest: "dist/example.js"
      }
    },
    watch: {
      files: [ "psrc/**/*.purs"
             , "bower_components/**/src/**/*.purs"],
      tasks: ["psc:all"]
    }
  });

  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("default", ["psc:all"]);
};
