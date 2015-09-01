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
define(["dojo/topic",
	    "dojo/router",
	    "dojo/hash",
		"dojo/cookie",
	    "rpg/log",
	    "rpg/soul",
	    "rpg/role",
	    "rpg/store"
], function (topic, router, hash, cookie, log, soul, role, store) {
	var port = "8080";
	var open = false;
	var opening = false;
	var ws;
	var messageQueue = [];
	// contains arrays of callbacks
	var listenerQueue = {};

	var ret = {};

	var queryListenerQueue = function (stmt, table, id) {
		id = parseInt(id) || id;
		var p = listenerQueue[stmt + ":" + id];
		if (p) {
			var row = store[table].get(id);
			p.forEach(function (callback) {
				callback(row);
			});
		}
	};

	// NOTE this function will be called on creation 
	// of an entity. Then, one waits until the server
	// pushes back all information via websocket.
	var listen = function (stmt, table, id, callback) {
		id = parseInt(id) || id;
		var row = store[table].get(id);
		if (row) {
			callback(row);
		} else {
			var p = listenerQueue[stmt + ":" + id];
			if (p) {
				p.push(callback);
			} else {
				listenerQueue[stmt + ":" + id] = [callback];
			}
		}
	};

	var on_view = function (stmt, payload) {
		if (stmt == "user_view") {
			store.user.rpg_insert_view(payload);
		} else if (stmt == "character_view") {
			store.group_chars.rpg_insert_view(payload);
		} else if (stmt == "group_view") {
			console.log("Returned \"group_view\" via ws.");
			return;
		} else if (stmt == "wiki_view") {
			var t = stmt.substr(0,stmt.indexOf("_"));
			store[t].rpg_insert_view(payload);
			// slug has another position
			queryListenerQueue(stmt, t, payload[4]);
			return;
		} else {
			// should be either "history", "image", "gallery" or "wiki"
			// "group_view" will not be called like this.
			var t = stmt.substr(0,stmt.indexOf("_"));
			store[t].rpg_insert_view(payload);
			queryListenerQueue(stmt, t, payload[0]);
		}
	};
	var on_create = function (stmt, payload) {
		if (stmt == "group_create") {
			// This is handled by role.group_create
		} else if (stmt == "character_create") {
			// this will only be called for group members.
			// The new char's owner gets a list
			var u = store.user.get(parseInt(payload[2]));
			var i = function(username) {
				store.group_chars.put({
					id : parseInt(item[0]),
					name : item[3],
					"short" : "",
					content : "",
					image : "",
					gallery : parseInt(item[1]),
					user_name : username,
					loaded : false
				});
			};
			if (!u) {
				ajax("user_view", [payload[0]], function(data) {
					store.user.rpg_insert_view(data);
					i(data[1]);
				});
			} else {
				i(u.name);
			}
		} else if (stmt == "group_gamemaster_add") {
			obj.group_gms.put({
				id : parseInt(item[0]),
				name : item[1]
			});

		}
	};
	var on_list = function (stmt, payload) {
		if (stmt == "history_order_list") {
			store.history.rpg_insert_order(payload);
		} else if (stmt == "image_order_list") {
			store.image.rpg_insert_order(payload);
		} else if (stmt == "tag_list") {
			store.tag.rpg_insert(payload);
		} else if (stmt == "permission_list") {
			store.permission.rpg_insert(payload);
		} else if (stmt == "own_characters_list") {
			store.own_chars.rpg_insert(payload);
		} else if (stmt == "group_gms_list") {
			store.group_gms.rpg_insert(payload);
		} else if (stmt == "own_gms_list") {
			store.own_gms.rpg_insert(payload);
		} else {
			console.error("Unknown statement " + stmt, message);
		}
	};
	var on_delete = function (stmt, payload) {
		if (typeof payload == "string") payload = parseInt(payload);
		if (stmt == "user_delete") {
			// NOTE actually, also here a character may will be 
			// deleted. So:
			// TODO listen in "char"
			if (payload == soul.user_id) {
				// TODO display some message to the user
				log.info("Your useraccount was deleted.");
				role.logout();
				return;
			} else {
				// actually, a query is to bloated, as there is a may of one gm matching
				store.group_gms
				.query(function (item) { return item.id == payload; })
				.forEach(function (item) { store.group_gms.remove(item.id); });
				store.group_chars
				.query(function (item) { return item.user_id == payload; })
				.forEach(function (item) { store.group_chars.remove(item.id); });
				// no return here, still delete from table "user"
			}
		} else if (stmt == "group_delete") {
			store.own_gms.remove(payload);
			store.own_chars
			.query(function (item) { return item.group_id == payload; })
			.forEach(function (item) { store.own_chars.remove(item.id); });
			if (payload == soul.group_id) {
				// TODO display some message to the user
				log.info("Your group was deleted.");
				role.choose_role();
			}
			return;
		} else if (stmt == "character_delete") {
			/* store.own_chars
			.query(function (item) { return item.id == payload; })
			.forEach(function (item) { store.own_chars.remove(item.id); });*/
			store.own_chars.remove(payload);
			/*store.group_chars
			.query(function (item) { return item.id == payload; })
			.forEach(function (item) { store.group_chars.remove(item.id); });*/
			store.group_chars.remove(payload);
			if (soul.character_id == payload) {
				log.info("Your character was deleted.");
				hash("");
				role.choose_role();
				return;
			}
			if (hash() == "/char/" + payload) {
				log.info("The character you looked at was deleted.");
				router.go("/timeline");
			}
			return;
		} else if (stmt == "gm_delete") {
			var u = parseInt(payload[0]);
			var g = parseInt(payload[1]);
			if (g == soul.group_id) {
				store.group_gms
				.query(function (item) { return item.id == u; })
				.forEach(function (item) { store.group_gms.remove(item.id); });
			} else if (u == soul.user_id) {
				store.own_gms
				.query(function (item) { return item.id == g; })
				.forEach(function (item) { store.group_gms.remove(item.id); });
			}
			return;
		} else if (stmt == "wiki_delete") {
			var h = hash();
			if (h.startsWith("/wiki/")) {
				var slug = h.substr(6);
				var w = store.wiki.get(slug);
				if (w.id == payload) {
					log.info("The wiki you looked at was deleted.");
					store.wiki.remove(slug);
					router.go("/wiki");
				}
			}
			return;
		} else if (stmt == "appointment_delete") {
			store.appointment.rpg_clear();
			return;
		}
		var t = stmt.substr(0,stmt.indexOf("_"));
		store[t].remove(payload);
		queryListenerQueue(stmt, t, payload);
	};

	var on_character_owner_edit = function(payload) {
		// payload : { user_id : <to_user> , character_id : <transferred char> }
		var c_id = parseInt(payload.character_id);
		var u_id = parseInt(payload.user_id);

		var gc = store.group_chars.get(c_id);
		var oc = store.own_chars.get(c_id);
		var i_lost_char = oc != null;
		if (oc != null /* == I probably lost the character*/) {
			if (soul.user_id === u_id) return; // transferd from me to me

			if (c_id == soul.character_id /* == I am currently in the role of the char */) {
				role.choose_role();
			}
			store.own_chars.remove(c_id);
		}
		if (gc != null /* == char is in this group */) {
			if (gc.user_id === u_id) return; // transferred to the same user

			gc.user_id = u_id;
			gc.user_name = store.user.get(u_id).name,
			store.group_chars.put(gc);
		}
		if (u_id == soul.user_id /* == I received the char */) {
			// do nothing. There shortly will arrive a "own_chars_list".
		}
	};

	var onmessage = function (message) {
		console.log("[ws] Received message: ", message);
		var delimiter = message.data.indexOf(":");
		var stmt = message.data.substr(0, delimiter).toLowerCase();
		var payload = JSON.parse(message.data.substr(delimiter + 1));

		if (stmt.endsWith("_view")) {
			on_view(stmt, payload);
		} else if (stmt.endsWith("_list")) {
			// tag_list, history_order_list, permission_list
			on_list(stmt, payload);
		} else if (stmt.endsWith("_delete")) {
			on_delete(stmt, payload);
		} else if (stmt.endsWith("_create") || stmt.endsWith("_add")) {
			on_create(stmt, payload);
		} else if (stmt === "character_owner_edit") {
			on_character_owner_edit(payload);
		} else if (stmt === "logout") {
			// some other instance called "logout", so simply close this window.
			ret.close();
			role.close();
		} else {
			console.error("Unknown statement " + stmt, message);
		}
	};
	var onopen = function () {
		open = true;
		opening = false;
		messageQueue.forEach (function(message) {
			ws.send(message);
		});
		messageQueue = [];
	};
	var onclose = function () {
		open = false;
		console.log("[ws] onclose called.");
	};
	// send encapsules opening of websocket
	ret.send = function (message) {
		if (!open) {
			messageQueue.push(message);
			if (!opening) {
				if (location.protocol == "http:") {
					console.log("[ws] Starting insecure websocket. (ws://" + location.host + "/ws)");
					ws = new WebSocket("ws://" + location.host + "/ws");
				} else {
					console.log("[ws] Starting secure websocket. (wss://" + location.host + "/ws)");
					ws = new WebSocket("wss://" + location.host + "/ws");
				}
				ws.onmessage = onmessage;
				ws.onopen = onopen;
				ws.onclose = onclose;
				opening = true;
			}
		} else {
			ws.send(message);
		}
	};
	ret.listen = listen;
	ret.close = function () { if (open) { ws.close(); open = false; }}
	/* var return_codes = {
		"user_edit" : {
			buffer : [],
			process : function (message) {
				// { id : <id>, name : <name> }
			}
		},
		"permission_update" : {
			buffer : [],
			process : function (message) {
				// { permission_broker : <id>, permission : <permission> }
			}
		}
	};*/

	// This will be called on logout
	topic.subscribe("logout", function () {
		// prevent infinity loop
		if (!open) return;

		ret.send("logout:");
		ret.close();
	});
	topic.subscribe("enter_user", function () {
		console.log("[ws] enter_user");
		ret.send("enter_user:" + cookie("AWS"));
	});
	topic.subscribe("enter_group", function (data) {
		if (data.old_group_id == -1) {
			console.log("[ws] enter_group");
			ret.send("enter_group:");
		} else {
			console.log("[ws] switch_group");
			if (data.old_group_id === undefined) {
				console.trace();
				console.log(data);
			}
			ret.send("switch_group:" + data.old_group_id);
		}
	});
	// TODO registry for looking up results of ajax -> look itself for store, otherwise enqueue and call onsuccess
	return ret;
});
