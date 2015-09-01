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
	    "dojo/dom-construct",
		"rpg/rndm"
], function (dom, dom_class, construct, rndm) {
	// NOTE : insertRow has to return the according DOM node.
	// TODO use a buffer to extend the dom more seldom. (with timeOut 100 ms, or even less)
	// NOTE insertRow always has to return some valid element to insert. If it should not be
	//		displayed, like in "user_rights.js", then add a class ("hidden") to it.
	return function (id, observable, insertRow, ondelete, onupdate, oninsert) {
		id = dom.byId(id);
		dom_class.add(id, "list");
		var listid = rndm();
		observable.forEach(function (object) {
			var item = insertRow(object);
			if (!item) return;
			// may preserve the old id
			object["data-list-" + listid] = item; // hold direct reference to DOM node ...
			construct.place(item, id, "last");
			if (typeof oninsert == "function") oninsert(object);
		});
		var handler = observable.observe(function(object, removedFrom, insertedInto) {
			if (insertedInto > -1 && removedFrom > -1) {
				var item = insertRow(object);
				if (!item) return;
				dom_class.add(item, "list-row");
				if (removedFrom == insertedInto ||
				  insertedInto == Array.prototype.indexOf.call(id.children, object["data-list-" + listid])
				   ) {
					construct.place(item, object["data-list-" + listid], "replace");
				} else {
					var len = id.children.length;
					if (len == insertedInto) {
						construct.place(item, id, "last");
					} else {
						if (removedFrom == -1) {
							construct.place(item, id, "last");
						} else {
							if (removedFrom > insertedInto) {
								construct.place(item, id.children[insertedInto], "before");
							} else {
								construct.place(item, id.children[insertedInto], "after");
							}
						}
					}
				}
				// TODO maybe on reorder a seperate event onreorder would be nice
				construct.destroy(object["data-list-" + listid]);
				object["data-list-" + listid] = item; // hold direct reference to DOM node ...
				if (typeof onupdate == "function") onupdate(object);
			} else if(removedFrom > -1){ // existing object removed
				construct.destroy(object["data-list-" + listid]);
				object["data-list-" + listid] = undefined;
				if (typeof ondelete == "function") ondelete(object);
			} else if (insertedInto > -1) {
				var item = insertRow(object);
				if (!item) return;
				var len = id.children.length;
				if (len == insertedInto) {
					construct.place(item, id, "last");
				} else {
					construct.place(item, id.children[insertedInto], "before");
				}
				object["data-list-" + listid] = item; // hold direct reference to DOM node ...
				if (typeof oninsert == "function") oninsert(object);
			} else { // new or updated object inserted
				console.error("[list.observe] Doesn't work");
			}
		}, true);

		return function() {
			// NOTE it is not important to delete the property,
			// the DOM node should be just unreferenced to get GCed
			dom_class.remove(id, "list");
			observable.forEach(function (item) { item["data-list-" + listid] = undefined; });
			handler.cancel();
			construct.empty(id);
			observable = null;
			insertRow = null;
			ondelete = null;
			onupdate = null;
			oninsert = null;
		};
	};
});
