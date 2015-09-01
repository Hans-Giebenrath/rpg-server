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

	on(dom.byId("play-nav"), "*:click", function(e) {
		var target = attr.get(this, "data-rpg-target");
		tabpane.prepare(target, this);
		router.go("/" + target);
	})
});
