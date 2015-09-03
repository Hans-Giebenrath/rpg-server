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

// assumption: every overlay is allowed to be closed at any time.
// Maybe this is shitty, if edits get lost. But an edit is only closed,
// if the character is taken away. And so, there is no way either to safe it.
// NOTE there is no destroy method, as overlays are not created randomly. There is
// a maximum number of overlay.
// a close after a close and an open after an open have no effect.
"use strict"
define(["dojo/dom",
	    "dojo/dom-construct",
		// "dojo/dom-style",
		"dojo/dom-class",
		"dojo/_base/lang",
		"dojo/topic",
		"dojo/on",
		"rpg/rndm",
		"dojo/domReady!"
], function(dom, construct, dom_class, lang, topic, on, rndm) {
	var module = {};
	var states = { closed : 0, open : 1};
	var attrs = {
		onshow : "rpg:mod-onshow",
		onclose : "rpg:mod-onclose",
		state : "rpg:mod-state"
	};
	var nodes = {
		pane : dom.byId("overlay"),
		content : dom.byId("content")
	};

	var currently_shown = 0;
	var all_overlays = [];
	var hide_pane = function() {
		dom_class.add(nodes.pane, "hidden");
		dom_class.remove(nodes.content, "hidden");
	};
	var show_pane = function() {
		dom_class.add(nodes.content, "hidden");
		dom_class.remove(nodes.pane, "hidden");
	};

	// this is required for immediate login,
	// when the modals are skipped.
	topic.subscribe("enter_user", function () {
		all_overlays.forEach(function(o) {
			module["close"](o);
		});
		hide_pane();
		/* if (currently_shown === 0) {
			hide_pane();
		}*/
	});
	topic.subscribe("enter_group", function () {
		all_overlays.forEach(function(o) {
			module["close"](o);
		});
		hide_pane();
		/* if (currently_shown === 0) {
			hide_pane();
		}*/
	});

	module["close"] = function(el) {
		if (el[attrs.state] === states.closed) return;
		el[attrs.state] = states.closed;

		currently_shown--;

		if (typeof el[attrs.onclose] === "function") el[attrs.onclose]();

		construct.place(el, "hidden-zone");
		if (currently_shown > 0) {
			dom_class.remove(nodes.pane.firstElementChild, "hidden");
		} else {
			hide_pane();
		}
	};

	module["closeOthers"] = function(el) {
		all_overlays.forEach(function(o) {
			if (o !== el) {
				module["close"](o);
			}
		});
	};
	
	module["show"] = function(el) {
		if (el[attrs.state] === states.open) return;
		el[attrs.state] = states.open;

		/* if (el[attrs.state] !== states.open) {
			currently_shown++;
		}*/

		if (typeof el[attrs.onshow] === "function") el[attrs.onshow]();

		dom_class.remove(el, "hidden");
		if (currently_shown > 0) {
			dom_class.add(nodes.pane.firstElementChild, "hidden");
		} else {
			show_pane();
		}
		construct.place(el, nodes.pane, "first");
		currently_shown++;
	};

	// takes an element into itself
	module["init"] = function(el, onshow, onclose) {
		el[attrs.state] = states.closed;
		el[attrs.onshow] = onshow;
		el[attrs.onclose] = onclose;
		all_overlays.push(el);
	};

	return module;
});
