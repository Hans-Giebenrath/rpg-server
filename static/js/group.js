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
	     "dojo/dom-construct",
	     "dojo/store/Memory",
		 "dojo/hash",
		 "dojo/on",
		 "dojo/router",
		 "dojo/topic",
		 "rpg/filteringselect",
	     "rpg/ajax",
		 "rpg/modal",
		 "rpg/tabpane",
		 "rpg/store",
		 "rpg/soul",
		 "rpg/list",
	     "dojo/domReady!"
], function (dom, attr, dom_class, construct, memory, hash, on, router, topic, filt, ajax, modal, tabpane, store, soul, list) {
	var nodes = {
		group_chars : dom.byId("group-chars-list"),
		group_gms : dom.byId("group-gms-list"),
		last_selected_char : null
	};
	var add_gm_modal;
	var add_char_modal;
	var delete_gm_modal;
	var delete_char_modal;
	add_char_modal = modal( // {{{
		"Add a user to your group",
		null,
		[{name : construct.create("div")},
		 {char_name : construct.toDom("<input type=\"text\" placeholder=\"New Character Name\" />")}
		],
		[{"Add Char" : function(inputs, close) {
			var user_id = inputs.filt.value();
			//var user_id = dijit.byId("user-selection-filter").get("value");
			var char_name = inputs.char_name.value;
			if (char_name == "") {
				inputs.char_name.setCustomValidity("Please supply name for char.");
				return;
			}
			inputs.char_name.setCustomValidity("");
			ajax("character_create", [user_id, char_name], close);
		}}, {"Cancel" : function(inputs, close) {
			close();
		}}], function(inputs) {
			inputs.filt = filt(inputs.name, store.user.query(), "id", "name");
			//construct.place(dijit.byId("user-selection-filter").domNode, inputs.name, "first");
		}, function(inputs) {
			inputs.filt.destroy();
			//construct.place(dijit.byId("user-selection-filter").domNode, "hidden-zone", "first");
		}
	); // }}}

	delete_char_modal = (function() { // {{{
		var character_id;
		var c;
		var on_delete;
		var m = modal(
			"Remove a character",
			{ desc : "Are you sure, that you want to delete the character from the group? This is just intendet for an inadvertently created character.", "class" : "small"},
			[{ div : construct.toDom("<span></span>"), label : "Chosen character"}],
			[{"Remove Char" : function(inputs, close) {
				ajax("character_delete", [character_id], function() {
					close();
					if (typeof on_delete === "function") on_delete();
				});
			}}, {"Cancel" : function(inputs, close) {
				close();
			}}], function onshow(inputs) {
				assert(soul.is_gm, "Only gms should be able to open the character removal modal.");
				inputs.div.textContent = c.name;
				m.title("Remove character \"" + c.name + "\"");
			}
		);

		return function (_character_id, _on_delete) {
			character_id = _character_id;
			on_delete = _on_delete;
			c = store.group_chars.get(_character_id)
			m.show();
		};
	})(); // }}}

	var character_config_modal = (function() { // {{{
		var c;
		var id;
		var m;
		var elements = [];
		elements.push((function() {
			var _d = construct.toDom("<input type=\"button\" value=\"Delete Character\" />");
			on(_d, "click", function() {
				// as the gm could delete it, it won't be closed automatically
				delete_char_modal(id, m.close);
			});
			return {
				"delete" : _d
			};
		})());
		elements.push((function() {
			var ret = construct.toDom("<div></div>");
			var f = filt(ret, store.user.query(), "id", "name");
			var btn = construct.toDom("<input type=\"button\" value=\"Transfer Ownership\" />");
			construct.place(btn, ret, "last");
			on(btn, "click", function() {
				ajax("character_owner_edit", [id, f.value(), c.user_id]/*, function(result) {
					// actually, chars cannot open it
					if (soul.is_char) {
						// the char transferred itself
						role.choose_role();
						// actually, unnecessary, as choose_role closes every other modal
						// close_modal();
					}
				}*/);
			});
			return { "transfer" : ret };
		})());
		m = modal(
			"Configure Character", null, elements, [{ "Close" : function (inputs, close) {
					close();
				} }], function onstart(inputs) {
					assert(soul.is_gm, "Only gms should be able to open this.");
					m.title("Configure Character \"" + c.name + "\"");
				}
		);
		return function (char_id) {
			c = store.group_chars.get(char_id);
			// make local closure for ajax call.
			id = char_id;
			m.show();
		};
	})(); // }}}


	add_gm_modal = modal( // {{{
		"Add another gm to your group",
		null,
		[{name : construct.create("div")}],
		[{"Add GM" : function(inputs, close) {
			var user_id = inputs.filt.value();
			//var user_id = dijit.byId("user-selection-filter").get("value");
			ajax("group_gamemaster_add", [user_id], close);
		}}, {"Cancel" : function(inputs, close) {
			close();
		}}], function(inputs) {
			inputs.filt = filt(inputs.name, store.user.query(), "id", "name");
			//construct.place(dijit.byId("user-selection-filter").domNode, inputs.name, "first");
		}, function(inputs) {
			inputs.filt.destroy();
			// construct.place(dijit.byId("user-selection-filter").domNode, "hidden-zone", "first");
		}
	); // }}}

	delete_gm_modal = (function() { // {{{
		var user_id;
		var m = modal(
			"Remove a gm",
			{ desc : "Are you sure, that you want to withdraw the user's role as gm from the group?", "class" : "small"},
			[{ div : construct.toDom("<span></span>"), label : "Chosen gm"}],
			[{"Remove GM" : function(inputs, close) {
				ajax("gm_delete", [user_id], close);
			}}, {"Cancel" : function(inputs, close) {
				close();
			}}], function onshow(inputs) {
				inputs.div.textContent = store.group_gms.get(user_id).name;
			}
		);

		return function (_user_id) {
			user_id = _user_id;
			m.show();
		};
	})(); // }}}

	var set_selected = function(el) {
		if (el !== null && el !== undefined) {
			dom_class.add(el, "selected");
			dom_class.remove(el, "selectable");
		}
		var last = nodes.last_selected_char;
		if (last !== null) {
			dom_class.add(last, "selectable");
			dom_class.remove(last, "selected");
		}
		nodes.last_selected_char = el;
	};

	var group_chars_list = list(nodes.group_chars, store.group_chars.query(), function(item) {
		// in general, this can be changed somehow
		var cur_char = false;
		if (hash() === "/char/" + item.id) {
			cur_char = true;
		}
		var ret = construct.create("div", {
			"data-rpg-role-type" : "char",
			"data-rpg-character-id" : item.id,
			"class" : (cur_char ? "selected lvl3" : "selectable lvl3")
		});
		if (cur_char) { set_selected(ret); }
		// don't fear XSS
		var ih = "<span>" + item.name + "</span><span><span class=\"info\">" + item.user_name + "</span>";
		if (soul.is_gm) {
			ih += "<input type=\"button\" value=\"Config\" class=\"configure\" data-id=\"" + item.id + "\" /></span>";
		} else {
			ih += "</span>";
		}
		ret.innerHTML = ih;
		return ret;
	});

	var group_gms_list = list(nodes.group_gms, store.group_gms.query(), function(item) {
		var cl = (item.id == soul.user_id) ? "emph lvl3" : "lvl3";
		var ret = construct.create("div", {
			"data-rpg-user-id" : item.id,
			"class" : cl
		});
		var ih = "<span>" + item.name + "</span>";
		if (soul.character_id == -1 && item.id != soul.user_id) {
			ih += "<input type=\"button\" value=\"Delete\" class=\"delete\" data-id=\"" + item.id + "\" />";
		}
		ret.innerHTML = ih;
		return ret;
	});

	(function() {
		// if we were on a char site, we want to deselect the nav entry, if we then
		// leave it.
		var char_handle;
		router.register("/char/:id*", function(arg) {
			if (!char_handle) {
				char_handle = topic.subscribe("/dojo/hashchange", function (changedHash) {
					// look, if we went to a char site.
					if (changedHash.startsWith("/char")) { return; }
					set_selected(null);
					char_handle.remove();
					char_handle = null;
					console.log("[group.char_listener] Removed listener.");
				});
			}
		});
	})();

	on(nodes.group_chars, "div.selectable:click", function(e) {
		tabpane.prepare("char", null);
		set_selected(this);
		router.go("/char/" + attr.get(this, "data-rpg-character-id"));
	});

	on(nodes.group_chars, ".configure:click", function(e) {
		e.preventDefault();
		e.stopPropagation();
		character_config_modal(parseInt(attr.get(this, "data-id")));
	});

	on(nodes.group_gms, ".delete:click", function(e) {
		e.preventDefault();
		e.stopPropagation();
		delete_gm_modal(parseInt(attr.get(this, "data-id")));
	});

	on(dom.byId("add-char"), "click", function (e) {
		add_char_modal.show();
	});

	on(dom.byId("add-gm"), "click", function (e) {
		add_gm_modal.show();
	});
});
