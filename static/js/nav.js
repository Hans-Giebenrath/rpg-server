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
		 "dojo/dom-attr",
		 "dojo/dom-class",
		 "dojo/debounce",
		 "dojo/query!css3",
		 "dojo/on",
		 "dojo/router",
		 "dojo/window",
		 "dojo/throttle",
		 "rpg/tabpane",
	     "dojo/domReady!"
], function(dom, attr, dom_class, debounce, query, on, router, win, throttle, tabpane) {
	var active = false;
	if (active) {
		var nodes = {
			play_nav : dom.byId("play-nav")
		};

		var offsetTop = nodes.play_nav.offsetTop;

		var check_height = function () {
			if (offsetTop == 0) {
				offsetTop = nodes.play_nav.offsetTop;
			}

			if (window.scrollY > offsetTop) {
				console.log("[nav.check_height] checked height and start loading.");
				dom_class.add(nodes.play_nav, "stick");
			} else {
				dom_class.remove(nodes.play_nav, "stick");
			}
		};

		var fun = throttle(check_height, 150);

		var signal = on(window, "scroll", fun);
	}

	on(dom.byId("play-nav"), ".selectable:click", function(e) {
		e.preventDefault();
		e.stopPropagation();
		var target = attr.get(this, "data-rpg-target");
		tabpane.prepare(target, this);
		router.go("/" + target);
		return false;
	})
});
