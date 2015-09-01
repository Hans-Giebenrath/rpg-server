"use strict"
define(["dojo/_base/lang",
	    "dojo/dom",
	    "dojo/dom-construct",
	    "dojo/store/Observable",
	    "dojo/store/Memory",
		"dojo/topic",
		"dojo/on",
		"dojo/router",
		"dojo/query",
		"dojo/hash",
		//"dijit/form/FilteringSelect",
		"rpg/list",
		"rpg/soul",
		"rpg/markdown",
		"rpg/ajax",
		"rpg/log",
], function(lang, dom, construct, observable, memory, topic, on, router, query, hash, list, soul, markdown, ajax, log) {
	var obj = {
		own_chars : new observable(new memory({})),
		own_gms : new observable(new memory({})),
		group_chars : new observable(new memory({})),
		group_gms : new observable(new memory({})),
		// group_gallery : new observable(new memory({})),
		// char_gallery : new observable(new memory({})),
		gallery : new observable(new memory({})), // filter by user_id
		history : new observable(new memory({})), // filter by user_id
		image : new observable(new memory({})),
		wiki : new observable(new memory({ idProperty : "slug" })),
		tag : new observable(new memory({})),
		user : new observable(new memory({})),
		permission : new observable(new memory({})),
		appointment : new observable(new memory({}))
	};

	var clear_store = function (store) {
		store.query().forEach(function(item) {
			store.remove(item[store.idProperty]);
		});
	};

	obj.own_chars.rpg_insert = function (coll) {
		coll.forEach(function (item) {
			var old = obj.own_chars.get(parseInt(item[0])) || {};
			obj.own_chars.put(lang.mixin(old, {
				id : parseInt(item[0]),
				name : item[1],
				// TODO use this fancy css-tooltip to display the short information
				"short" : markdown.display(item[2], "ocs" + item[0]),
				group_id : parseInt(item[3]),
				group_name : item[4]
			}));
		});
	};

	// TODO maybe this is useless.
	obj.own_chars.rpg_insert_view = function (item) {
		// updating will be done internally.
		var old = obj.own_chars.get(parseInt(item[0])) || {};
		obj.own_chars.put(lang.mixin(old, {
			id : parseInt(item[0]),
			user : item[1],
			group_id : parseInt(item[2]),
			gallery : parseInt(item[3]),
			name : item[4],
			image : item[5],
			short : markdown.display(item[6], "ocs" + item[0]),
			short_raw : item[7],
			content : markdown.display(item[8], "occ" + item[0]),
			content_raw : item[9]
		}));
	};

	obj.own_gms.rpg_insert = function (coll) {
		coll.forEach(function (item) {
			var old = obj.own_gms.get(parseInt(item[0])) || {};
			obj.own_gms.put(lang.mixin(old, {
				id : parseInt(item[0]), // group_id
				name : item[1], // group_name
				gallery : parseInt(item[2]) // gallery_broker_id
			}));
		});
	};

	obj.group_chars.rpg_insert = function (coll, gallery_loaded) {
		// if a character enters after login, we will be informed about
		// gallery changes. So this should then be true.
		coll.forEach(function (item) {
			var old = obj.group_chars.get(parseInt(item[0])) || {};
			obj.group_chars.put(lang.mixin(old, {
				id : parseInt(item[0]),
				name : item[1],
				"short" : markdown.display(item[2], "gcs" + item[0]),
				content : markdown.display(item[3], "gcc" + item[0]),
				image : item[4],
				gallery : parseInt(item[5]),
				user_id : item[6],
				user_name : item[7],
				loaded : !!((item[0] == soul.char_id) || gallery_loaded)
			}));
		});
	};

	obj.group_chars.rpg_insert_view = function (item) {
		// updating will be done internally.
		var old = obj.group_chars.get(parseInt(item[0])) || {};
		obj.group_chars.put(lang.mixin(old, {
			id : parseInt(item[0]),
			user_id : parseInt(item[1]),
			user_name : obj.user.get(parseInt(item[1])).name,
			group : parseInt(item[2]),
			gallery : parseInt(item[3]),
			name : item[4],
			image : item[5],
			short : markdown.display(item[6], "gcs" + item[0]),
			short_raw : item[7],
			content : markdown.display(item[8], "gcc" + item[0]),
			content_raw : item[9],
			loaded : true
		}));
	};

	obj.group_gms.rpg_insert = function (coll) {
		coll.forEach(function (item) {
			var old = obj.group_gms.get(parseInt(item[0])) || {};
			obj.group_gms.put(lang.mixin(old, {
				id : parseInt(item[0]),
				name : item[1]
			}));
		});
	};

	obj.group_chars.rpg_get = function (id, onsuccess) {
		var c = obj.group_chars.get(id);
		if (!c.loaded) {
			ajax("character_view", [c.id], function (data) {
				obj.group_chars.rpg_insert_view(data);
				if (typeof onsuccess == "function") onsuccess(obj.group_chars.get(id));
			}, onerror);
		} else {
			if (typeof onsuccess == "function") onsuccess(c);
		}
	};

	obj.wiki.rpg_insert = function (coll) {
		coll.forEach(function (item) {
			var tags = [];
			if (item[7]) {
			   tags = item[7].split(",").map(function (tag) {return parseInt(tag);});
			}
			obj.wiki.put({
				id : parseInt(item[0]),
				title : item[1],
				slug : item[2],
				"short" : markdown.display(item[3], "ws" + item[0]),
				created : item[4],
				permission : parseInt(item[5]),
				permission_broker : parseInt(item[6]),
				tags : tags, // integerarray
				pb_loaded : false
			});
			// do not require in here permissions - this would not scale for a big bunch of wikis.
		});
	};

	obj.wiki.rpg_insert_view = function (item) {
		// updating will be done internally.
		var tags = [];
		if (item[10]) {
		   tags = item[10].split(",").map(function (tag) {return parseInt(tag);});
		}
		var old = obj.wiki.get(item[4]) || {};
		obj.wiki.put(lang.mixin(old, {
			id : parseInt(item[0]),
			permission : parseInt(item[1]),
			permission_broker : parseInt(item[2]),
			created : item[3],
			slug : item[4], // NOTE on update, update 6 lines above, too.
			title : item[5],
			short : markdown.display(item[6], "ws" + item[0]),
			short_raw : item[7],
			content : markdown.display(item[8], "wc" + item[0]),
			content_raw : item[9],
			tags : tags, // integerarray
			loaded : true
		}));
	};

	obj.tag.rpg_insert = function (coll) {
		coll.forEach(function (item) {
			var old = obj.tag.get(parseInt(item[0])) || {};
			obj.tag.put(lang.mixin(old, {
				id : parseInt(item[0]),
				tag : item[1]
			}));
		});
	};

	obj.gallery.rpg_insert = function (coll, images_loaded) {
		coll.forEach(function (item) {
			obj.gallery.put({
				id : parseInt(item[0]),
				title : item[1],
				content : markdown.display(item[2], item[0] + "/gc" + item[0]),
				gallery_broker : parseInt(item[3]),
				permission : parseInt(item[4]),
				permission_broker : parseInt(item[5]),
				image_url : item[6],
				// character_id : character_id,
				images_loaded : images_loaded,
				pb_loaded : false
			});
		});
	};

	obj.gallery.rpg_insert_view = function (item) {
		var old = obj.gallery.get(parseInt(item[0])) || {};
		obj.gallery.put(lang.mixin(old, {
			id : parseInt(item[0]),
			permission : parseInt(item[1]),
			permission_broker : parseInt(item[2]),
			gallery_broker : parseInt(item[3]),
			title : item[4],
			content : markdown.display(item[5], item[0] + "/gc" + item[0]),
			content_raw : item[6],
			image_url : item[7],
			loaded : true
		}));
	};

	obj.image.rpg_insert = function (coll, gallery) {
		coll.forEach(function (item) {
			var old = obj.image.get(parseInt(item[0])) || {};
			obj.image.put(lang.mixin(old, {
				id : parseInt(item[0]),
				order : parseInt(item[1]),
				title : item[2],
				content : markdown.display(item[3], gallery + "/" + item[0] + "/ic" + item[0]),
				url : item[4],
				orig : item[5],
				permission : parseInt(item[6]),
				permission_broker : parseInt(item[7]),
				gallery : parseInt(gallery),
				pb_loaded : false
			}));
		});
	};

	obj.image.rpg_insert_view = function (item) {
		var old = obj.image.get(parseInt(item[0])) || {};
		obj.image.put(lang.mixin(old, {
			id : parseInt(item[0]),
			gallery : parseInt(item[1]),
			permission : parseInt(item[2]),
			permission_broker : parseInt(item[3]),
			url : item[4],
			orig : item[5],
			title : item[6],
			// actually i have no idea, how a toc should work in here.
			content : markdown.display(item[7], item[1] + "/" + item[0] + "/ic" + item[0]),
			content_raw : item[8],
			loaded : true
		}));
	};

	obj.image.rpg_insert_order = function (coll) {
		coll.forEach(function (item) {
			var old = obj.image.get(parseInt(item[0]));
			obj.image.put(lang.mixin(old, {
				id : parseInt(item[0]),
				order : parseInt(item[1])
			}));
		});
	};

	obj.user.rpg_insert = function (coll) {
		coll.forEach(function (item) {
			obj.user.put({
				id : parseInt(item[0]),
				name : item[1]
			});
		});
	};

	obj.user.rpg_insert_view = function (item) {
		var old = obj.user.get(parseInt(item[0])) || {};
		obj.user.put(lang.mixin(old, {
			id : parseInt(item[0]),
			name : item[1]
		}));
	}

	obj.permission.rpg_insert = function (coll) {
		coll.forEach(function (item) {
			var old = obj.permission.query({character_id : parseInt(item[0]), permission_broker : parseInt(item[1])});
			if (old) {
				if (old.total > 1) { console.error("There are duplicate permissions."); }
				if (old.total == 1)	{
					old = old.reduce(function(prev, item) { return item; });
				}
			} else { old = {}; }
			obj.permission.put(lang.mixin(old, {
				character_id : parseInt(item[0]),
				permission_broker : parseInt(item[1]),
				permission : parseInt(item[2])
			}));
		});
	};

	obj.appointment.rpg_insert = function (coll) {
		// TODO check, if here a ["rpg-list-id"] is overwritten
		coll.forEach(function (item) {
			obj.appointment.rpg_insert_view(item);
		});
	};

	obj.appointment.rpg_insert_view = function (item) {
		var old = obj.appointment.get(parseInt(item[0])) || {};
		var electors = [];
		if (item[3]) {
			electors = item[3].split(",").map(function(item) { return parseInt(item); });
		}
		var elected;
		switch (item[2]) {
			case "f" : elected = false; break;
			case "t" : elected = true; break;
		    default: throw "[store.appointment.rpg_insert_view] Unknown state for \"elected\".";
		}
		if (elected) {
			obj.appointment.query({elected : true}).forEach(function(item) {
				item.elected = false;
				obj.appointment.put(item);
			});
		}
		obj.appointment.put(lang.mixin(old, {
			id : parseInt(item[0]),
			date : item[1],
			elected : elected, // TODO look for boolean
			electors : electors
		}));
	};

	obj.appointment.rpg_clear = function () {
		clear_store(obj.appointment);
	};

	obj.history.rpg_insert = function (coll) {
		// TODO check, if here a ["rpg-list-id"] is overwritten
		coll.forEach(function (item) {
			obj.history.put({
				id : parseInt(item[0]),
				title : item[1],
				short : markdown.display(item[2], item[0] + "/hs" + item[0]),
				created : item[3],
				date_ingame : item[4],
				date_outgame : item[5],
				order : parseInt(item [6]),
				permission : parseInt(item[7]),
				permission_broker : parseInt(item[8]),
				pb_loaded : false,
				loaded : false
			});
		});
	};

	obj.history.rpg_insert_view = function (item) {
		var old = obj.history.get(parseInt(item[0])) || {};
		obj.history.put(lang.mixin(old, {
			id : parseInt(item[0]),
			permission : parseInt(item[1]),
			permission_broker : parseInt(item[2]),
			created : item[3],
			date_ingame : item[4],
			date_outgame : item[5],
			order : parseInt(item[6]),
			title : item[7],
			short : markdown.display(item[8], item[0] + "/hs" + item[0]),
			short_raw : item[9],
			content : markdown.display(item[10], item[0] + "/hc" + item[0]),
			content_raw : item[11],
			loaded : true
		}));
	};

	obj.history.rpg_insert_order = function (coll) {
		// TODO check, if here a ["rpg-list-id"] is overwritten
		coll.forEach(function (item) {
			// old should not be undefined
			var old = obj.history.get(parseInt(item[0]));
			obj.history.put(lang.mixin(old, {
				id : parseInt(item[0]),
				order : parseInt(item[1])
			}));
		});
	};

	obj.history.rpg_get = function (id, onsuccess, onerror) {
		var c = obj.history.get(id);
		if (!c.loaded) {
			ajax("history_view", [c.id], function (data) {
				obj.history.rpg_insert_view(data);
				if (typeof onsuccess == "function") onsuccess(obj.history.get(id));
			}, onerror);
		} else {
			if (typeof onsuccess == "function") onsuccess(c);
		}
	};

	obj.permission.rpg_get = function (table, id, onsuccess) {
		// NOTE as the permissions are always queries by the 
		// user_permissions itself, do not return it here. Just signal.
		// NOTE this is done for slugs. As the slug is not convertable,
		// parseInt returns NaN. In this case, the slug will be preserved.
		id = parseInt(id) || id;
		var c = obj[table].get(id);
		if (c.pb_loaded) {
			if (typeof onsuccess == "function") onsuccess();
			return;
		}
		// Already check here, so there won't be further ajax calls
		c.pb_loaded = true;
		ajax("permission_list", [c.permission_broker], function (data) {
			obj.permission.rpg_insert(data || []);
			if (typeof onsuccess == "function") onsuccess();
		}, function () { c.pb_loaded = false; });
	};

	obj.wiki.rpg_get = function (slug, onsuccess, onerror, createwiki) {
		var c = obj.wiki.get(slug);
		if (!c) {
			createwiki(slug, onsuccess);
		} else if (!c.loaded) {
			ajax("wiki_view", [c.id], function (data) {
				obj.wiki.rpg_insert_view(data);
				onsuccess(obj.wiki.get(slug));
			}, onerror);
		} else {
			onsuccess(c);
		}
	};

	obj.gallery.rpg_get = function (id, onsuccess, onerror) {
		id = parseInt(id);
		var c = obj.gallery.get(id);
		if (!c.loaded) {
			ajax("gallery_view", [c.id], function (data) {
				obj.gallery.rpg_insert_view(data);
				onsuccess(obj.gallery.get(id));
			}, onerror);
		} else {
			onsuccess(c);
		}
	};

	obj.image.rpg_get = function (id, onsuccess, onerror) {
		id = parseInt(id);
		var c = obj.image.get(id);
		if (!c.loaded) {
			ajax("image_view", [c.id], function (data) {
				obj.image.rpg_insert_view(data);
				onsuccess(obj.image.get(id));
			}, onerror);
		} else {
			onsuccess(c);
		}
	};

	topic.subscribe("logout", function() {
		Object.keys(obj).forEach(function(key) {
			clear_store(obj[key]);
		});
	});

	topic.subscribe("enter_group", function(data) {
		clear_store(obj.group_chars);
		clear_store(obj.group_gms);
		clear_store(obj.gallery);
		clear_store(obj.image);
		clear_store(obj.history);
		clear_store(obj.wiki);
		clear_store(obj.tag);
		clear_store(obj.permission);
		clear_store(obj.appointment);

		if (!data) {
			log.error("Something went wrong, I guess.");
			return;
		}
		if (data.group) {
			obj.group_chars.rpg_insert(data.group.chars);
			obj.group_gms.rpg_insert(data.group.gms);
			obj.wiki.rpg_insert(data.group.wiki);
			obj.tag.rpg_insert(data.group.tag);
			obj.gallery.rpg_insert(data.group.gallery, false);
			obj.history.rpg_insert(data.group.history);
			obj.appointment.rpg_insert(data.group.appointment);
		}
	});
	topic.subscribe("enter_user", function(data) {
		if (data === null || data === undefined) { return; }
		obj.own_chars.rpg_insert(data.own.chars);
		obj.own_gms.rpg_insert(data.own.gms);
		obj.user.rpg_insert(data.user);
	});

	// create a new filter for userselection
	//filt("user-selection-filter", obj.user, "name");
	/*(new filt({
		id: "user-selection-filter",
		store: obj.user,
		searchAttr: "name"
	}, "user-selection-filter")).startup();*/

	router.register(/\/gallery\/(\w*)/, function(arg) {
		var c = obj.gallery.get(arg.params[0]);
		if (!c) {

		}
		if (c.images_loaded) {
			return;
		}
		c.images_loaded = true;
		ajax("image_list", [c.id], function (data) {
			obj.image.rpg_insert(data || [], c.id);
		});
		obj.permission.rpg_get("gallery", c.id);
	});

	router.register("/history/:id", function(arg) {
		if (!arg.params.id || arg.params.id == "-1") { return; }
		obj.permission.rpg_get("history", parseInt(arg.params.id));
	});

	router.register(/\/wiki\/(\w*)/, function(arg) {
		try {
			obj.permission.rpg_get("wiki", arg.params[0]);
		} catch (err) {
			/*
			* The wiki-page does not yet exist. wiki.js will handle an unknown wiki-page.
			* This should be a nice shorthand, if in a text are already links defined.
			*/
		}
	});

	router.register(/\/image\/(\w*)/, function(arg) {
		obj.permission.rpg_get("image", parseInt(arg.params[0]));
	});

	return obj;
});
