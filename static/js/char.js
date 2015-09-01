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
		 "dojo/dom-style",
		 "dojo/dom-attr",
		 "dojo/dom-construct",
		 "dojo/router",
		 "dojo/on",
		 "dojo/store/Memory",
		 "dojo/request",
		 "dojo/topic",
		 "rpg/soul",
		 "rpg/modal",
		 "rpg/filteringselect",
		 "rpg/tabpane",
		 "rpg/markdown",
		 "rpg/log",
		 "rpg/ajax",
		 "rpg/store",
		 "dojo/NodeList-dom",
		 "dojo/domReady!"
], function(dom, dom_class, style, attr, construct, router, on, memory, request, topic, soul, modal, filt, tabpane, markdown, log, ajax, store) {
	var last_loaded_id = null;
	var nodes = {
		image : dom.byId("char-profile"),
		name : dom.byId("char-name"),
		short : dom.byId("char-summary").children[1],
		content : dom.byId("char-content"),
		edit : dom.byId("char-edit")
	};
	var update_content = function(data) {
		nodes.image.src = data.image || "";
		nodes.name.textContent = data.name;
		nodes.content.innerHTML = data.content;
		if (soul.character_id == data.id) {
			dom_class.remove(nodes.edit, "hidden");
		} else {
			dom_class.add(nodes.edit, "hidden");
		}
	};

	var character_modify_modal = (function() { // {{{
		var c;
		var id;
		var m = modal(
			"Edit Character", null, [
				{ name : construct.toDom("<input type=\"text\" />")},
				{ short : construct.toDom("<textarea type=\"text\" class=\"short\" />")},
				{ content : construct.toDom("<textarea type=\"text\" />")}
				], [{ "Save" : function oneditcharacter(inputs, close) {
					// no ws needed, as content will be updated 'automagically' by observer
					ajax("character_edit", [id, inputs.name.value, markdown.convert(inputs.content.value), inputs.content.value, markdown.convert(inputs.short.value), inputs.short.value], close);
				} }, { "Discard" : function oncancel(inputs, close) {
					close();
				} }], function onstart(inputs) {
					inputs.name.value = c.name;
					inputs.short.value = c.short_raw;
					inputs.content.value = c.content_raw;
				}, function onclose(inputs) {
				}
		);
		return function (char_id) {
			c = store.group_chars.get(char_id);
			// make local closure for ajax call.
			id = char_id;
			m.show();
		};
	})(); // }}}

	on(nodes.edit, "click", function (e) {
		character_modify_modal(last_loaded_id);
	});

	on(nodes.content, ".toc div:click", function (e) {
		router.go("/char/" + last_loaded_id + "/" + attr.get(this, "data-a"));
	});
	var display = (function() { // {{{
		var obs;
		var set_obs = function (c) {
			if (obs) obs.cancel();
			var id = c.id;
			nodes.name.textContent = c.name;
			nodes.short.innerHTML = c.short;
			nodes.content.innerHTML = c.content;
			var q = store.group_chars.query({id : id})
			obs = q.observe(function(object, removedFrom, insertedInto) {
				if ((removedFrom > -1 && insertedInto > -1) || insertedInto == 0) {
						nodes.name.textContent = object.name;
						nodes.short.innerHTML = object.short;
						nodes.content.innerHTML = object.content;
				} else if (removedFrom > -1) {
					// wiki got deleted, go to grid
					nodes.name.textContent = "";
					nodes.short.innerHTML = "";
					nodes.content.innerHTML = "";
				} else {
					console.error("[char.display.set_obs] Multiple chars?");
				}
			}, true);
		}
		return function (id, anchor) {
			if (soul.group_id < 0) {
				return;
			}
			tabpane.prepare("char");
			store.group_chars.rpg_get(id, function(data) {
				if (last_loaded_id !== id) {
					last_loaded_id = id;
					set_obs(data);
					if (id != soul.character_id && soul.is_char) {
						style.set(nodes.edit, "visibility", "hidden");
					} else {
						style.set(nodes.edit, "visibility", "visible");
					}
				}

				if (anchor) {
					var n = dom.byId(anchor);
					if (n) n.scrollIntoView();
				}
				tabpane.change("char");
			});
		};
	})(); // }}}
	// topic.subscribe("logout", function() {
		// update_content({image : "", name : "", content : ""});
	// });
	//router.register(/\/char(?:\/(\w+)(?:\/(\w+))?)?/, function(arg) {
	router.register("/char/:id", function(arg) {
		display(arg.params.id);
	});
	router.register("/char/:id/:anchor", function(arg) {
		display(arg.params.id, arg.params.anchor);
	});
});
