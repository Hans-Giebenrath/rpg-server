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
		 "dojo/debounce",
	     "dojo/router",
		 "dojo/query!css3",
		 "dojo/on",
		 "dojo/topic",
		 "dojo/window",
		 "rpg/store",
		 "rpg/soul",
		 "rpg/tabpane",
		 "rpg/user_rights",
		 "rpg/permission_mapping",
		 "rpg/modal",
		 "rpg/orderable",
		 "rpg/markdown",
		 "rpg/ajax",
		 "rpg/ws",
		 "rpg/btns",
		 "rpg/list",
		 "dojo/NodeList-dom",
	     "dojo/domReady!"
], function(dom, dom_class, style, attr, construct, debounce, router, query, on, topic, win, store, soul, tabpane, user_rights, permission_mapping, modal, orderable, markdown, ajax, ws, btns, list) {
	var nodes = { // {{{
		btns : dom.byId("history-btns"),
		list : dom.byId("history-timelist"),
		placeholder : dom.byId("history-placeholder"),
		content : dom.byId("history-content"),
		detail : dom.byId("history-detail")
	} // }}}
	var history_edit_modal = (function () { // {{{
		var h;
		var m = modal("Edit history", null, 
					  [
						  { title : construct.toDom("<input type=\"text\" />") },
						  { date_ingame : construct.toDom("<input type=\"text\" />") },
						  { date_outgame : construct.toDom("<input type=\"text\" />") },
						  { permission : construct.toDom("<div></div>") },
						  { short : construct.toDom("<textarea class=\"short\" />") },
						  { content : construct.toDom("<textarea />") }
					  ], [
					  {
						  "Save" : function (inputs, close) {
							  ajax(
								  "history_edit",
								  // NOTE h.permission_broker is necessary for automatic push of permissions to clients.
								  [h.id, inputs.title.value, inputs.date_ingame.value, inputs.date_outgame.value, markdown.convert(inputs.short.value), inputs.short.value, markdown.convert(inputs.content.value), inputs.content.value, inputs.rpg_character_rights.get_permissions(), h.permission_broker], close
							  );
						  }
					  },{
						  "Discard" : function (inputs, close) { close(); }
					  }
					  ], function onstart(inputs) {
						  inputs.rpg_character_rights = user_rights(inputs.permission, h.permission_broker, "history", h.id);
						  inputs.title.value = h.title;
						  inputs.date_ingame.value = h.date_ingame;
						  inputs.date_outgame.value = h.date_outgame;
						  inputs.short.value = h.short_raw;
						  inputs.content.value = h.content_raw;
					  }, function onclose(inputs) {
						  inputs.rpg_character_rights.destroy();
					  }
					 )
		return function (history_id) {
			store.history.rpg_get(history_id, function (_h) {
				h = _h;
				m.show();
			});
		};
	})(); // }}}
	var history_create_modal = (function () { // {{{
		var m = modal("Create a new Wikisite", null, 
					  [
						  { title : construct.toDom("<input type=\"text\" placeholder=\"Title for History\" />") },
						  { date_ingame : construct.toDom("<input type=\"text\" placeholder=\"When did it happen ingame?\" />") },
						  { date_outgame : construct.toDom("<input type=\"text\" placeholder=\"When did it happen outgame?\" />") },
						  { permission : construct.toDom("<div></div>") },
						  { short : construct.toDom("<textarea placeholder=\"Short description\" class=\"short\" />") },
						  { content : construct.toDom("<textarea placeholder=\"Content for History\" />") }
					  ], [
					  {
						  "Create" : function (inputs, close) {
							  ajax(
								  "history_create_with_permission",
								  [inputs.title.value, inputs.date_ingame.value, inputs.date_outgame.value, markdown.convert(inputs.short.value), inputs.short.value, markdown.convert(inputs.content.value), inputs.content.value, inputs.rpg_character_rights.get_permissions()],
								  function (data) {
									  ws.listen("history_view", "history", data[0], function(_data) {
										  router.go("/history/" + data[0]);
										  close();
									  });
								  }
							  );
						  }
					  },{
						  "Cancel" : function (inputs, close) { close(); }
					  }
					  ], function onstart(inputs) {
						  inputs.rpg_character_rights = user_rights(inputs.permission);
					  }, function onclose(inputs) {
						  inputs.rpg_character_rights.destroy();
					  }
					 )
		return function () {
			m.show();
		};
	})(); // }}}
	var history_delete_modal = (function() { // {{{
		var history_id;
		var m = modal(
			"Delete History", {desc : "Are you sure you want to delete it?", "class" : "small"}, [ ], [
				{ "Delete History" : function (inputs, close) {
			ajax("history_delete", [history_id], close);
		} }, { "Cancel" : function oncancel(inputs, close) {
			close();
		} }]
		);
		return function (_history_id) {
			history_id = _history_id;
			m.show();
		};
	})(); // }}}

	var preventScroll = true;
	var insertListRow = function (item) {
		return construct.toDom(
			"<div class=\"history-list-entry\" id=\"history-list-entry-" + item.id + "\" data-id=\"" + item.id + "\"><h5 class=\"title\">" +
			item.title + 
			"</h5><div class=\"date-ingame\">" +
			item.date_ingame + 
			"</div><div class=\"date-outgame\">" +
			item.date_outgame + 
			"</div></div>"
		);
	};
	var reorder = function (o) {
		ajax("history_order_edit", [o.getOrder(true)]);
	};
	var orderableHandle = orderable(nodes.list, store.history.query({}, {sort : [{attribute : "order", descending : true }]}), insertListRow, debounce(reorder, 800), 0);

	var insertDetailRow = function (item) {
		var ret = construct.toDom(
			"<div class=\"history-detail-entry\" id=\"history-detail-entry-" + item.id + "\" data-id=\"" + item.id + "\">" + 
			"<h1 class=\"title\">" +
			item.title + 
			"</h1><div class=\"date-ingame\">" +
			item.date_ingame + 
			"</div><div class=\"date-outgame\">" +
			item.date_outgame + 
			"</div><div class=\"summary\"><div class=\"header\">Summary</div><div class=\"content\">" +
			item.short +
			"</div></div><div class=\"content\">" +
			item.content +
			"</div></div>"
		);
		btns(ret, [ {
			attributes : { "id" : item.id },
			"class" : "edit",
			label : "Edit History",
			permission_type : "edit",
			permission : item.permission,
		},{
			attributes : { "id" : item.id },
			"class" : "delete",
			label : "Delete History",
			permission_type : "delete",
			permission : item.permission
		}]);
		return ret;
	};

	on(nodes.detail, ".delete:click", function(e) { // {{{ button events
		history_delete_modal(parseInt(attr.get(this, "data-id")));
		e.stopPropagation();
		e.preventDefault();
	});
	on(nodes.detail, ".edit:click", function(e) {
		history_edit_modal(parseInt(attr.get(this, "data-id")));
		e.stopPropagation();
		e.preventDefault();
	}); // }}}

	on(nodes.list, ".history-list-entry:click", function(e) {
		e.stopPropagation();
		e.preventDefault();
		var id = attr.get(this, "data-id");
		preventScroll = false;
		console.log("[history.detail-list.click] Going to " + id);
		router.go("/history/" + id);
	});


	// var detail_list_destroy = function () {};
	/* var detail_list_destroy =*/ list(nodes.detail, store.history.query({loaded : true}, {sort : [{attribute : "order", descending : true }]}), insertDetailRow);

	/* list_count_listeners = */ store.history.query({loaded : true}).observe(function(object, removedFrom, insertedInto) {
		if (removedFrom > -1 && insertedInto == -1) {
			// something was deleted
			if (store.history.data.length == 0) {
				dom_class.remove(nodes.placeholder, "hidden");
				dom_class.add(nodes.content, "hidden");
			}
		} else if (removedFrom == -1 && insertedInto > -1) {
			// something was inserted
			if (store.history.data.length == 1) {
				dom_class.add(nodes.placeholder, "hidden");
				dom_class.remove(nodes.content, "hidden");
			}
		}
	});

	topic.subscribe("enter_group", function () {
		preventScroll = true;
		// detail_list_destroy();
		// detail_list_destroy = list(nodes.detail, store.history.query({loaded : true}, {sort : [{attribute : "order", descending : true }]}), insertDetailRow);
		construct.empty(nodes.btns);
		// TODO clear list and detail view
		btns(nodes.btns, [ {
			"class" : "add",
			label : "Add new History",
			permission_type : "add",
			onclick : history_create_modal
		}]);
		if (soul.character_id != -1) {
			style.set(nodes.btns, "visibility", "hidden");
		}
		if (store.history.data.length > 0) {
			dom_class.add(nodes.placeholder, "hidden");
			dom_class.remove(nodes.content, "hidden");
		} else {
			dom_class.remove(nodes.placeholder, "hidden");
			dom_class.add(nodes.content, "hidden");
		}

	});

	var scr = function (id, anchor) {
		if (anchor) {
			dom.byId(id + "/" + anchor).scrollIntoView();
		} else {
			dom.byId("history-detail-entry-" + id).scrollIntoView();
		}
	};
	var load_hist = function (item, scrollTo, anchor) {
		if (!preventScroll && item.id == scrollTo) {
			if (item.loaded) {
				scr(item.id, anchor);
			} else {
				store.history.rpg_get(item.id, function () {
					scr(item.id, anchor);
				});
			}
		} else if (!item.loaded) {
			store.history.rpg_get(item.id);
		}
	}

	var check_height = function () {
		var b = win.getBox();
		var body = document.body;
		var html = document.documentElement;
		var height = Math.max( body.scrollHeight, body.offsetHeight, 
							  html.clientHeight, html.scrollHeight, html.offsetHeight );
		// this formula may is wrong
		if (height - b.t < b.h + 100) {
			console.log("[history.check_height] checked height and start loading.");
			store.history.query({loaded : false}, {
				sort : [{attribute : "order", descending : true}],
				count : 5
			}).forEach(function (item) {
				load_hist(item);
			});
		}
	};

	var load_to_id = function (id, anchor) {
		var h = store.history.get(parseInt(id));
		if (!h) { return; }
		store.history.query({}, {
				sort : [{attribute : "order", descending : true}]
		}).filter(function (item) {
			return item.order > h.order - 5;
		}).forEach(function (item) {
			load_hist(item, id, anchor);
		});
	};

	var scroll_listener = (function () {
		var scrolled = false;

		var to = null;
		var create_to = function() {
			to = window.setInterval(function () {
				if (scrolled) {
					scrolled = false;
					return;
				}

				check_height();
				window.clearInterval(to);
				to = null;
			}, 300);
		};

		var signal;
		return {
			activate : function () {
				if (signal) { return; }
				signal = on(window, "scroll", function(e) {
					scrolled = true;
					if (!to) {
						create_to();
					}
				});

			},
			remove : function () {
				if (!signal) { return; }
				signal.remove();
				signal = null;
				scrolled = false;
				if (to) {
					window.clearInterval(to);
					to = null;
				}
			}
		};
	})();

	on(nodes.content, ".toc div:click", function (e) {
		router.go("/history/" + attr.get(this, "data-a"));
	});

	router.register("/history", function () {
		var last = store.history.query().reduce(function (prev, item) {
			if (!prev) { return item; }
			if (prev.order > item.order) {
				// bigger order means later in history
				return prev;
			} else {
				return item;
			}
		}, {order : -1, id : -1});
		console.log("[history.router] Going to " + last.id);
		router.go("/history/" + last.id);
	});
	var handle;
	router.register("/history/:id", function (arg) {
		if (arg.params.id == "-1") {
			// there is no history, yet
		} else {
			load_to_id(arg.params.id);
		}
		console.log("[history.router/] Went to " + arg.params.id);
		tabpane.change("history");
		scroll_listener.activate();
		if (!handle) {
			handle = router.register(/^\/(?!history)/, function () {
				scroll_listener.remove();
				console.log("[history] Removes scroll listener.");
				handle.remove();
				handle = null;
			});
		}
	});
	router.register("/history/:id/:anchor", function (arg) {
		preventScroll = false;
		load_to_id(arg.params.id, arg.params.anchor);
	});
});
