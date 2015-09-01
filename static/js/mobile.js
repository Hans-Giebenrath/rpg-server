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

require(["dojo/dom",
		 "dojo/dom-class",
		 "dojo/on",
		 "dojo/hash",
		 "dojo/topic",
		 "dojo/domReady!"
], function(dom, dom_class, on, hash, topic) {
	nodes = {
		cb1 : dom.byId("all-my-roles-cb"),
		cb2 : dom.byId("this-group-cb"),
		cb3 : dom.byId("misc-cb")
	};

	topic.subscribe("/dojo/hashchange", function(changedHash){
		nodes.cb1.checked = false;
		nodes.cb2.checked = false;
		nodes.cb3.checked = false;
	});

	nodes.cb1.onchange = function() {
		nodes.cb2.checked = false;
		nodes.cb3.checked = false;
	};
	nodes.cb2.onchange = function() {
		nodes.cb1.checked = false;
		nodes.cb3.checked = false;
	};
	nodes.cb3.onchange = function() {
		nodes.cb1.checked = false;
		nodes.cb2.checked = false;
	};
});
