/*
	Copyright (C) 2015 Hans Giebenrath

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>
*/

"use strict"
require(["dojo/dom",
		 "dojo/dom-class",
		 "dojo/on",
		 "dojo/domReady!"
], function(dom, dom_class, on) {
	var nodes = {
		body : document.body,
		toggler : dom.byId("switch-theme")
	};

	var storageKey = "rpg-theme";

	var themes = ["clean", "colour"];
	var idx = (function() {
		var i = 0;
		for (i = 0; i < themes.length; i++) {
			if (dom_class.contains(nodes.body, themes[i])) {
				idx = i;
				break;
			}
		}
		return i;
	})();

	var save = (function() {
		if (localStorage) {
			return function(theme) {
				localStorage.setItem(storageKey, theme);
			};
		} else {
			return function() {};
		}
	})();

	if(localStorage) {
		var storedTheme = localStorage.getItem(storageKey);
		if (storedTheme) {
			var idx2 = themes.indexOf(storedTheme);
			if (idx2 != -1 && idx2 != idx) {
				dom_class.remove(nodes.body, themes[idx]);
				idx = idx2;
				dom_class.add(nodes.body, themes[idx]);
			}
		}
	}

	on(nodes.toggler, "click", function(e) {
		dom_class.remove(nodes.body, themes[idx]);
		idx = (idx + 1) % themes.length;
		dom_class.add(nodes.body, themes[idx]);
		save(themes[idx]);
	});
});
