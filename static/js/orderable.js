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
	    "dojo/on",
	    "dojo/query",
	    "dojo/dom-construct",
	    "dojo/dom-attr",
	    "dojo/dom-class",
		"rpg/btns",
		"rpg/list"
], function (dom, on, query, construct, attr, dom_class, btns, list) {
	var _insertRow = function (domNode, permission) {
		var container = construct.toDom("<div class=\"orderable-row\"></div>");
		construct.place(domNode, container, "first");
		btns(container, [ {
			"class" : "up",
			label : "Up",
			permission_type : "edit",
			permission : permission
		}, {
			"class" : "down",
			label : "Down",
			permission_type : "edit",
			permission : permission
		}]);
			 
		return container;
	};

	return function (container, observable, insertRow, onreorder, permission) {
		// map shall return an array containing the ids
		var manipulated = false;
		var ret;
		dom_class.add(container, "orderable");
		var destroy_list = list(container, observable, function (item) { return _insertRow(insertRow(item), permission); } );
		var signal_up = on(container, ".up:click", function (e) {
			e.stopPropagation();
			e.preventDefault();
			var c = this.parentNode.parentNode; // or parentElement?
			var prev = c.previousElementSibling;
			if (prev) {
				construct.place(c, prev, "before");
				onreorder(ret);
			}
		});
		var signal_down = on(container, ".down:click", function (e) {
			e.stopPropagation();
			e.preventDefault();
			var c = this.parentNode.parentNode; // or parentElement?
			var next = c.nextElementSibling;
			if (next) {
				construct.place(c, next, "after");
				onreorder(ret);
			}
		});

		ret = {
			destroy : function () {
				dom_class.remove(container, "orderable");
				signal_up.remove();
				signal_down.remove();
				destroy_list();
			},
			getOrder : function (reverse) {
				var counter = 0;
				var q = query("> *", container);
				if (reverse) {
					return "{{" + q.map(
						function (node) {
							return attr.get(node.firstElementChild, "data-id");
						}).map(function(el) { return el + "," + (q.length - (counter++));}).join("},{") + "}}";
				} else {
					return "{{" + q.map(
						function (node) {
							return attr.get(node.firstElementChild, "data-id");
						}).map(function(el) { return el + "," + (++counter);}).join("},{") + "}}";
				}
			}
		};

		return ret;
	};
});
