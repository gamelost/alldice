module.exports = function(grunt) {
	"use strict";

	grunt.initConfig({
		srcFiles: [
			"psrc/**/*.purs"
			, "bower_components/**/src/**/*.purs"
			, "bower_components/**/src/**/*.purs.hs"
		],

		dotPsci: ["<%=srcFiles%>"],

		psc: {
			options: {
				main: "Example.Main",
				modules: ["Example.Main"]
			},
			all: {
				src: [ "<%=srcFiles%>" ],
				dest: "dist/example.js"
			}
		},
		watch: {
			src: [ "<%=srcFiles%>" ],
			tasks: ["psc:all"]
		},
	});

	grunt.loadNpmTasks('grunt-contrib-watch');
	grunt.loadNpmTasks("grunt-purescript");

	grunt.registerTask("default", ["psc:all", "dotPsci"]);
};
