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
		 "dojo/dom-attr",
		 "dojo/dom-construct",
	     "dojo/router",
		 "dojo/query!css3",
		 "dojo/on",
		 "dojo/topic",
		 "rpg/store",
		 "rpg/tabpane",
		 "rpg/tagselect",
		 "rpg/user_rights",
		 "rpg/permission_mapping",
		 "rpg/soul",
		 "rpg/list",
		 "rpg/modal",
		 "rpg/markdown",
		 "rpg/ajax",
		 "rpg/ws",
		 "dojo/NodeList-dom",
	     "dojo/domReady!"
], function(dom, dom_class, attr, construct, router, query, on, topic, store, tabpane, tagselect, user_rights, permission_mapping, soul, list, modal, markdown, ajax, ws) {
	var columns = {
		//slug : "Slug",
		title : "Title",
		"short" : "Short Description"
		//created : "Created"
		// TODO size
	};
	var nodes = { // {{{
		view : dom.byId("wiki-view"),
		overview : dom.byId("wiki-overview"),
		add : query("input.add", "wiki-pane"), // there are two buttons
		edit : dom.byId("wiki-view-edit"),
		"delete" : dom.byId("wiki-view-delete"),
		title : dom.byId("wiki-view-title"),
		"short" : dom.byId("wiki-view-summary").children[1],
		content : dom.byId("wiki-view-content"),
		grid : dom.byId("wiki-grid"),
		tbody : dom.byId("wiki-grid").tBodies[0]
	} // }}}

	// NOTE as tags are only a small array, we will just use a simple forEach approach to test subset-ity. There is also a O(m+n) approach.
	var selected_tags;
	var createList = (function () { // {{{
		var insertRow = function(item) {
			var ret = Object.keys(columns).reduce(function(prev, col) { return prev + "<td class=\"selectable\">" + item[col].replace(/(<([^>]+)>)/ig,"") + "</td>"; }, "<tr data-slug=\"" + item.slug + "\">") + "</tr>";
			return construct.toDom(ret);
		};
		var lastList = null;
		var tag_filter = function (item) {
			return selected_tags.list_id().every(function(tag) {
				return item.tags.indexOf(tag) > -1;
			});
		};
		var sort = {attribute : "title", descending : false};
		var descAttr = function (isDesc) {return isDesc ? "desc" : "asc";};
		return function (tableHead) {
			if (typeof lastList == "function") lastList();
			if (tableHead) {
				var _sort = attr.get(tableHead, "data-sort");
				dom_class.remove("wiki-grid-head-" + sort.attribute, descAttr(sort.descending));
				sort.descending = (_sort == sort.attribute) ? !sort.descending : false;
				sort.attribute = _sort;
				dom_class.add(tableHead, descAttr(sort.descending));
			}
			lastList = list(nodes.tbody, store.wiki.query(
				(selected_tags.list_id().length > 0) ? tag_filter : null,
				{ sort : [ sort ] }
			), insertRow);
		};
	})(); // }}}
	// Currently does not work. Forgot to push stuff.
	selected_tags = tagselect("wiki-overview-tags", createList);

	var current_slug = "";
	var wiki_edit_modal = (function () { // {{{
		var w;
		var m = modal("Edit Wikisite", null, 
					  [
						  { title : construct.toDom("<input type=\"text\" />") },
						  // { slug : construct.toDom("<input type=\"text\" />") },
						  { permission : construct.toDom("<div></div>") },
						  { tags : construct.toDom("<div></div>") },
						  { short : construct.toDom("<textarea class=\"short\" />") },
						  { content : construct.toDom("<textarea />") }
					  ], [
					  {
						  "Edit" : function (inputs, close) {
							  ajax(
								  "wiki_edit",
								  // NOTE w.permission_broker is necessary for automatic push of permissions to clients.
								  [w.id, inputs.title.value, markdown.convert(inputs.short.value), inputs.short.value, markdown.convert(inputs.content.value), inputs.content.value, inputs.rpg_tags.list_sql(), inputs.rpg_character_rights.get_permissions(), w.permission_broker], close // NOTE reload of wikipage is done via observable.
							  );
						  }
					  },{
						  "Cancel" : function (inputs, close) { close(); }
					  }
					  ], function onstart(inputs) {
						  inputs.rpg_character_rights = user_rights(inputs.permission, w.permission_broker, "wiki", w.slug);
						  inputs.rpg_tags = tagselect(inputs.tags, null, true);
						  inputs.rpg_tags.preselect(store.tag.query(function(t) { 
						  	return w.tags.indexOf(t.id) > -1; 
						  }));
						  inputs.title.value = w.title;
						  // inputs.slug.value = w.slug;
						  inputs.short.value = w.short_raw;
						  inputs.content.value = w.content_raw;
					  }, function onclose(inputs) {
						  inputs.rpg_character_rights.destroy();
						  inputs.rpg_tags.destroy();
					  }
					 )
		return function () {
			// oncreated should be called with the newly created stuff from the server -> use topic listening for that.
			store.wiki.rpg_get(current_slug, function (_w) {
				w = _w;
				m.show();
			});
		};
	})(); // }}}
	var wiki_create_modal = (function () { // {{{
		var _oncreated;
		var _slug;
		var _go_back;
		var m = modal("Create a new Wikisite", null, 
					  [
						  { title : construct.toDom("<input type=\"text\" placeholder=\"Title for Wikisite\" />") },
						  { slug : construct.toDom("<input type=\"text\" id=\"wiki-modal-slug-input\" placeholder=\"Slug for Wikisite\" />") },
						  { permission : construct.toDom("<div></div>") },
						  { tags : construct.toDom("<div></div>") },
						  { short : construct.toDom("<textarea placeholder=\"Short description\" class=\"short\" />") },
						  { content : construct.toDom("<textarea placeholder=\"Content for Wikisite\" />") }
					  ], [
					  {
						  "Create" : function (inputs, close) {
							  ajax(
								  "wiki_create_with_permission",
								  [inputs.title.value, inputs.slug.value, markdown.convert(inputs.short.value), inputs.short.value, markdown.convert(inputs.content.value), inputs.content.value, inputs.rpg_tags.list_sql(), inputs.rpg_character_rights.get_permissions()],
								  function (data) {
									  ws.listen("wiki_view", "wiki", inputs.slug.value, function(_data) {
										  if (!_data.slug || (_data.slug == inputs.slug.value)) {
											  _oncreated(_data, inputs.slug.value);
										  } else {
											  console.log("[wiki_create ws callback] slugs do not match: inputs.slug.value: " + inputs.slug.value + ", _data.slug: " + _data.slug, _data);
										  }
										  close();
									  });
								  }
							  );
						  }
					  },{
						  "Cancel" : function (inputs, close) {
							  close();
							  if(_go_back === true) {
								  window.history.go(-1);
							  }
						  }
					  }
					  ], function onstart(inputs) {
						  inputs.rpg_character_rights = user_rights(inputs.permission);
						  inputs.rpg_tags = tagselect(inputs.tags, null, true);
						  if (typeof _slug === 'string' && _slug !== '') {
							  inputs.slug.disabled = true;
							  inputs.slug.value = _slug;
						  } else {
							  inputs.slug.disabled = false;
							  inputs.slug.value = '';
						  }
					  }, function onclose(inputs) {
						  inputs.rpg_character_rights.destroy();
						  inputs.rpg_tags.destroy();
					  }
					 )
		return function (slug, oncreated, go_back) {
			// oncreated should be called with the newly created stuff from the server -> use topic listening for that.
			_slug = slug || "";
			_oncreated = oncreated;
			_go_back = go_back;
			m.show();
		};
	})(); // }}}
	var wiki_delete_modal = (function() { // {{{
		var w;
		var m = modal(
			"Delete Wikisite", {desc : "Are you sure you want to delete it?", "class" : "small"}, [ ], [
				{ "Delete Wikisite" : function (inputs, close) {
			ajax("wiki_delete", [w.id], function (e) {
				// store.wiki.remove(w.slug);
				close();
				router.go("/wiki");
			});
		} }, { "Cancel" : function oncancel(inputs, close) {
			close();
		} }]
		);
		return function () {
			w = store.wiki.get(current_slug);
			m.show();
		};
	})(); // }}}
	nodes.add.on("click", function (e) {
		// the data-arg contains the data of the newly created wiki. It is already stored (?)
		wiki_create_modal("", function (data, slug) { router.go("/wiki/" + slug); }, false);
	});
	on(nodes.edit, "click", function(e) {
		wiki_edit_modal();
	});
	on(nodes["delete"], "click", function(e) {
		wiki_delete_modal();
	});
	(function () {
		var colgrp = "<colgroup>";
		var header = "<tr>";
		Object.keys(columns).forEach(function(col) {
			colgrp += "<col class=\"" + col + "\">";
			header += "<th class=\"selectable\" id=\"wiki-grid-head-" + col + "\" data-sort=\"" + col + "\">" + columns[col] + "</th>";
		});
		colgrp += "</colgroup>";
		header += "</tr>";
		nodes.grid.tHead.innerHTML = header;
		nodes.grid.children[0].innerHTML = colgrp;
	})();
	query("> thead th", nodes.grid).on("click", function(e) {
		createList(this);
	});
	on(nodes.tbody, "tr:click", function (e) {
		var slug = attr.get(this, "data-slug");
		router.go("/wiki/" + slug);
	});

	on(nodes.content, ".toc div:click", function (e) {
		router.go("/wiki/" + current_slug + "/" + attr.get(this, "data-a"));
	});

	var display = (function() {
		var last_slug = "";
		var obs;
		var set_obs = function (w) {
			if (obs) obs.cancel();
			var slug = w.slug;
			nodes.title.textContent = w.title;
			nodes.short.innerHTML = w.short;
			nodes.content.innerHTML = w.content;
			if (soul.is_gm) {
				dom_class.remove(nodes.edit, "hidden");
				dom_class.remove(nodes["delete"], "hidden");
			} else {
				switch (w.permission) {
					case permission_mapping.none.value:
					case permission_mapping.read.value:
						dom_class.add(nodes.edit, "hidden");
						dom_class.add(nodes["delete"], "hidden");
						break;
					case permission_mapping.edit.value:
					case permission_mapping.add.value:
						dom_class.remove(nodes.edit, "hidden");
						dom_class.add(nodes["delete"], "hidden");
						break;
					case permission_mapping["delete"].value:
					case permission_mapping.master.value:
						dom_class.remove(nodes.edit, "hidden");
						dom_class.remove(nodes["delete"], "hidden");
						break;
				}
			}
			var q = store.wiki.query({slug : slug})
			obs = q.observe(function(object, removedFrom, insertedInto) {
				if ((removedFrom > -1 && insertedInto > -1) || insertedInto == 0) {
						nodes.title.textContent = object.title;
						nodes.short.innerHTML = object.short;
						nodes.content.innerHTML = object.content;
				} else if (removedFrom > -1) {
					// wiki got deleted, go to grid
					nodes.title.textContent = "";
					nodes.short.innerHTML = "";
					nodes.content.innerHTML = "";
					display("grid", last_slug);
				} else {
					console.error("[wiki.display.set_obs] Multiple wikis?");
				}
			}, true);
		}
		return function (whichone, slug, anchor) {
			if (whichone == "grid") {
				dom_class.add(nodes.view, "hidden");
				dom_class.remove(nodes.overview, "hidden");

				// Do not delete here last_slug, as it may be reselected
				tabpane.change("wiki", true);
			} else {
				store.wiki.rpg_get(slug, function(data, new_slug) {
					if (new_slug) { slug = new_slug; } // update for creation from false hash
					if (last_slug != slug) {
						last_slug = slug;
						set_obs(data);
					}
					dom_class.remove(nodes.view, "hidden");
					dom_class.add(nodes.overview, "hidden");

					// Do the change already here.
					// If it is done after the if, then there is no content
					// to scroll to. Thus, first show, then go to content.
					tabpane.change("wiki");

					if (anchor) {
						var node = dom.byId(anchor);//query("[rpg-anchor=\"" + anchor + "\"]")[0];
						if (node) { node.scrollIntoView(); }
					} else {
						nodes.title.scrollIntoView();
					}
				}, null, function(slug, on_created) { wiki_create_modal(slug, on_created, true); });
			}
		};
	})();
	topic.subscribe("enter_group", function() {
		createList();
	});
	router.register("/wiki", function() {
		display("grid");
		current_slug = "";
	});
	router.register("/wiki/:slug", function(arg) {
		current_slug = arg.params.slug;
		display("view", arg.params.slug);
	});
	router.register("/wiki/:slug/:anchor", function(arg) {
		current_slug = arg.params.slug;
		display("view", arg.params.slug, arg.params.anchor);
	});
});
