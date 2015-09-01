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
define(["dojo/dom-construct",
	    "dojo/query!css3",
		"dojo/dom-attr",
		"dojo/dom-class",
		"dojo/on",
	    "rpg/ajax",
	    "rpg/list",
	    "rpg/soul",
	    "rpg/permission_mapping",
		"rpg/rndm",
		"rpg/store"
], function(construct, query, attr, dom_class, on, ajax, list, soul, permission_mapping, rndm, store) {
	// Best would be to call it in modal.onstart
	// 1. permission_broker id, needed for retrieving
	// 2. container
	// 3. for which table the permission will be updated
	// 4. id of the table
	// TODO somehow secure, that for every char/perm_broker is a value. Maybe during insertion in the memory, or server side.
	/*var create_options = function (permission, character_id) {
		var ret = construct.create("select", { "rpg-character-id" : character_id })
		Object.keys(permission_mapping).forEach(function(item) {
			var tmp = construct.toDom("<option value=\"" + permission_mapping[item].value + "\">" + permission_mapping[item].label + "</option>");
			construct.place(tmp, ret, "last");
			if (permission == permission_mapping[item].value) {
				tmp.selected = "selected";
			}
		});
		return ret;
	};*/

	var cb_name = function(character_id, entity_id) {
		return "ur-cb-" + character_id + "-" + entity_id;
	};

	return function (container, permission_broker, table, entity_id) {
		var ret;
		var inner_cont;
		// ajax should be done by store, if we navigate there
		var character_ids = {};

		// NOTE pulling out of this function is not really possible, as we in here keep track of which character is listed.
		var create_options = function (permission, character_id) {
			var r = rndm();
			return Object.keys(permission_mapping).reduce(function(prev, item) {
				if (item == "add") return prev;

				var checked = "";
				if (permission == permission_mapping[item].value) {
					checked = "checked=\"checked\"";
					character_ids[character_id] = permission;
				}
				return prev + "<td class=\"rotate\"><input data-character-id=\"" + character_id + "\" value=\"" + permission_mapping[item].value + "\" type=\"radio\" name=\"" + r + "\" " + checked + " /></td>";
			}, "");
		};

		inner_cont = construct.place(
			"<table id=\"" + rndm() + "\" class=\"permission-select\" >" +
				"<tr>" +
					"<th>Character</th>" +
					(Object.keys(permission_mapping).reduce(function(prev, item) {
						if (item == "add") return prev;
						return prev + "<th class=\"rotate\" ><div class=\"rotate\"><span class=\"rotate\">" + permission_mapping[item].label + "</span></div></th>";
					}, "")) +
				"</tr>" +
			"</table>" , container, "first");
		//ret = list(inner_cont, store.permission.query({"permission_broker" : permission_broker}), function(item) {
		var onHandler = on(inner_cont, "input[type=\"radio\"]:click", function(e) {
			var character_id = attr.get(this, "data-character-id");
			character_ids[character_id] = this.value;
		});
		var _destroy;
		if (permission_broker == -1 || !permission_broker) {
			// maybe this is taken, if a new wiki-slug is opened?
			_destroy = list(inner_cont, store.group_chars.query(function(item) {
				return item["id"] != soul.character_id;
			}), function(item) {
				var character_name = item.name;
				var row = construct.toDom("<tr><td>" + character_name + "</td>" + create_options(permission_mapping["none"].value, item.id) + "</tr>");
				construct.place(row, inner_cont, "last");
				// be restrictive with default permission ...
				return row;
			});
		} else if (!table) {
			// only permission_broker supplied -> this means, that permission_broker is the default permission
			_destroy = list(inner_cont, store.group_chars.query(function(item) {
				return item["id"] != soul.character_id;
			}), function(item) {
				var character_name = item.name;
				var row = construct.toDom("<tr><td>" + character_name + "</td>" + create_options(permission_broker, item.id) + "</tr>");
				construct.place(row, inner_cont, "last");
				return row;
			});

		} else {
			/*if (table)*/ store.permission.rpg_get(table, entity_id, function() {
				_destroy = list(inner_cont, store.permission.query(function(item) {
					return item["permission_broker"] == permission_broker && item["character_id"] != soul.character_id;
				}), function(item) {
					var character_name = store.group_chars.get(item.character_id).name;
					var row;
					if (table == "image") {
						var gallery_perm_broker = store.gallery.get(store.image.get(entity_id).gallery).permission_broker;
						var gallery_perm = store.permission.query({ permission_broker : gallery_perm_broker, character_id : item.character_id})[0].permission;
						if (gallery_perm >= permission_mapping["edit"]) {
							tmp.disabled = true;
							row = construct.toDom("<tr class=\"hidden\"></tr>");
						} else {
							row = construct.toDom("<tr><td>" + character_name + "</td>" + create_options(item.permission, item.character_id) + "</tr>");
						}
					} else {
						row = construct.toDom("<tr><td>" + character_name + "</td>" + create_options(item.permission, item.character_id) + "</tr>");
					}
					construct.place(row, inner_cont, "last");
					return row;
				});
			});
		}
		ret = {
			destroy : function () {
				_destroy();
				construct.destroy(inner_cont);
				onHandler.remove();
				character_ids = null;
				create_options = null;
			},
			get_permissions_json : function () {
				return character_ids;
			},
			save : function (onsuccess) {
				var perms = this.get_permissions_json();
				store.permission.query({ permission_broker : permission_broker }).forEach(function (item) {
					if (perms[item.character_id] != item.permission) {
						ajax("permission_update", [permission_broker, item.character_id, perms[item.character_id]], onsuccess);
					}
				});
			},
			get_permissions : function () {
				var j = this.get_permissions_json();
				var ret = "{{" + Object.keys(j).map(function(key) { return key + "," + j[key]}).join("},{") + "}}";
				if (ret === "{{}}") ret = "";
				return ret;
			}
		};

		return ret;
	};
});
