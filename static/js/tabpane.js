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
define(["dojo/dom",
	    "dojo/dom-class",
	    "dojo/query",
		"rpg/log",
		"dojo/NodeList-dom",
		"dojo/domReady!"
], function(dom, dom_class, query, log) {
	/*
	* First call prepare - this will enable passing
	* of the clicked tab. Or in case of "char" passing null.
	* In theory, cool would be a loading screen in the future,
	* or something that shows, that currently the pane is loading
	*/
	var last_target	= "";
	var prepared_target = "";
	var last_tab = null;
	var last_pane = null;

	var new_tab = null;
	var new_pane = null;

	var active_class = "selected";
	return {
		prepare : function(target) {
			if (target !== "char") {
				new_tab = dom.byId("play-nav-" + target);
			} else {
				new_tab = null;
			}
			new_pane = dom.byId(target + "-pane");
			prepared_target = target;
		},
		change : function(target, silent) {
			if (target !== prepared_target) {
				if (silent) {
					console.info("Tabbing explicitly prevented. target: " + target + ", prepared_target: " + prepared_target);
					return;
				} else {
					console.warn("Tabbing error. target: " + target + ", prepared_target: " + prepared_target);
					this.prepare(target);
				}
			}

			// here we have to test for same target
			if (last_tab && last_tab != new_tab) {
				dom_class.remove(last_tab, active_class);
			}
			if (last_pane && last_pane != new_pane) {
				dom_class.remove(last_pane, active_class);
			}

			if(new_tab) {
				dom_class.add(new_tab, active_class);
			}
			dom_class.add(new_pane, active_class);

			last_target = target; // do not set in prepare
			last_pane = new_pane;
			last_tab = new_tab;
		},
		current : function (full) {
			if (full && last_target == "char") {
				return "character";
			} else {
				return last_target;
			}
		}
	};
});
