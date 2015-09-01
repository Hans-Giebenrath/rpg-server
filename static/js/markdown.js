"use strict"
define(["dojo/query"
], function (query) {
	// TODO if a section, which would be added to the toc, is inside a <priv>, then it too has to
	// be surrounded with <priv> tags in the toc.
	
	var parseTOC = function(text, id) {
		// TODO as this function will only be called after retrieval,
		// we don't have to take care for any <priv> tags.
		// look for every minified (lowercase, replace spaces) heading, if it was already matched.
		// Append some stuffediestuff.
		// Also, look if there is some marked-stuff on headings (names?)
		var toc = "<div class=\"toc\">";
		var counter = { 1 : 0, 2 : 0, 3 : 0, 4 : 0, 5 : 0, 6 : 0};
		var inc_counter = function (level) {
			level = parseInt(level);
			counter[level]++;
			var i;
			for (i = 1; i < level; i++) { if (!counter[i]) { counter[i]++; }}
			for (i = level + 1; i <= 6; i++) { counter[i] = 0; }
		};
		var counter_to_string = function () {
			return Object.keys(counter).reduce(function (prev, k) { return prev + (counter[k] ? counter[k] + "." : ""); }, "");
		};

		var append_heading = function (id, text, level) {
			inc_counter(level);
			toc = toc + "<div data-a=\"" + id + "\" class=\"selectable lvl" + level + "\" data-num=\"" + counter_to_string() + "\">" + text + "</div>";
		};

		text = text.replace(/<h([1-6]) id="(.*?)">(.*?)<\/h[1-6]>/g, function (match, level, _id, text, offset) {
			append_heading(id + "-" + _id, text, level);
			return match.replace(_id, id + "-" + _id);
		});

		toc = toc + "</div>";

		return text.replace(/\[TOC\]/g, toc);
	}
	return {
		convert : marked, // TODO something is wrong here. This variable does not exist.
		/*convert : (function() {
			var md = window.markdown('full', {
				html: true,
				linkify: true,
				typographer: true,
			});
			return md.render;
		})(),*/
		display : function (text, id) {
			if (text.indexOf('[TOC]') > -1) {
				text = parseTOC(text, id);
			}
			return text;
		}
	};
});
