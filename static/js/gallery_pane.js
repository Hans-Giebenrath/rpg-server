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
	    "dojo/dom-construct",
		"dojo/dom-attr",
		"dojo/dom-class",
		"dojo/on",
		"dojo/router",
		"dojo/query!css3",
	    "rpg/ajax",
		"rpg/list",
		"rpg/log",
		"rpg/store",
		"rpg/modal",
		"rpg/user_rights",
		"rpg/btns",
		"rpg/imgslider",
		"rpg/imgurlconverter",
		"rpg/markdown",
		"rpg/file_uploader",
		"dojo/domReady!"
], function(dom, construct, attr, dom_class, on, router, query, ajax, list, log, store, modal, user_rights, btns, imgslider, imgurlconverter, markdown, file_uploader) {
	// TODO 
	// Heavy bullshit. I don't know, maybe the whole context stuff is a little bit too complicated?
	// returns a function that takes the following args:
	// 1. id of the container, where to store
	// 2. Observable
	// 3. "character" or "gallery"
	// 4. if "character" supplied for 2., than: "/char/<id>" (without trailing /)
	// 5. variable to keep track - update will only be applied, if this variable changes
	//

	// this will be updated everytime, if a new character is chosen, or gallery tab.

		var gallery_create_modal = (function() { // {{{
			// maybe this closure is not necessary ...
			var _method = "";
			var m = modal(
				"Create Gallery", null, [
					{ title : construct.toDom("<input type=\"text\" placeholder=\"Title\" />")},
					{ content : construct.toDom("<textarea type=\"text\" placeholder=\"Description\" />")},
					{ permission : construct.toDom("<div></div>") }], [
					{ "Create Gallery" : function oncreategallery(inputs, close) {
						ajax(_method, [inputs.title.value, markdown.convert(inputs.content.value), inputs.content.value, inputs.rpg_character_rights.get_permissions()], close);
					} }, { "Cancel" : function oncancel(inputs, close) {
						close();
					} }], function onstart(inputs) {
						inputs.rpg_character_rights = user_rights(inputs.permission); // TODO
					}, function onclose(inputs) {
						inputs.rpg_character_rights.destroy();
					}
			);
			return function (method_prefix) {
				_method = method_prefix + "_gallery_create_with_permission";
				m.show();
			};
		})(); // }}}
		var gallery_modify_modal = (function() { // {{{
			var g;
			var m = modal(
				"Edit Gallery", null, [
					{ title : construct.toDom("<input type=\"text\" />")},
					{ content : construct.toDom("<textarea type=\"text\" />")},
					{ permission : construct.toDom("<div></div>") }], [
					{ "Save" : function oneditgallery(inputs, close) {
						// NOTE g.permission_broker is necessary for automatic push of permissions to clients.
						ajax("gallery_edit", [g.id, inputs.title.value, markdown.convert(inputs.content.value), inputs.content.value, inputs.rpg_character_rights.get_permissions(), g.permission_broker], close);
						// as gallery already exists, we do not need to wait.
					} }, { "Cancel" : function oncancel(inputs, close) {
						close();
					} }], function onstart(inputs) {
						inputs.rpg_character_rights = user_rights(inputs.permission, g.permission_broker, "gallery", g.id); // TODO
						inputs.title.value = g.title;
						inputs.content.value = g.content_raw;
					}, function onclose(inputs) {
						inputs.rpg_character_rights.destroy();
					}
			);
			return function (gallery_id) {
				store.gallery.rpg_get(gallery_id, function(_g) {
					g = _g;
					m.show();
				});
			};
		})(); // }}}
		var gallery_delete_modal = (function() { // {{{
			var gallery_id;
			var m = modal(
				"Delete Gallery", {desc : "Are you sure, you want to delete it?", "class" : "small"}, [ ], [
					{ "Delete Gallery" : function ondeletegallery(inputs, close) {
				ajax("gallery_delete", [gallery_id], close);
			} }, { "Cancel" : function oncancel(inputs, close) {
				close();
			} }]
			);
			return function (_gallery_id) {
				gallery_id = _gallery_id;
				m.show();
			};
		})(); // }}}

		var image_create_modal = (function() { // {{{
			// maybe this closure is not necessary ...
			var _gallery_id;
			var m = modal(
				"Create an Image", null, [
					{ image : construct.toDom("<input type=\"file\" />")},
					{ title : construct.toDom("<input type=\"text\" placeholder=\"Title\" />")},
					{ content : construct.toDom("<textarea type=\"text\" placeholder=\"Description\" />")},
					{ setcover : construct.toDom("<input type=\"checkbox\">Use as gallery cover</input>")},
					{ permission : construct.toDom("<div></div>") }], [
					{ "Create Image" : function oncreategallery(inputs, close) {
						// TODO file-uploading -> this also would lead to place the next "ajax" into its onsuccess handler
						file_uploader(inputs.image, function (result) {
							ajax("image_create_with_permission", [_gallery_id, inputs.title.value, markdown.convert(inputs.content.value), inputs.content.value, result, inputs.image.files[0].name, inputs.rpg_character_rights.get_permissions()], function onsuccess(data) {
								if (inputs.setcover.checked) {
									ajax("gallery_set_image", [data[0]], close);
								} else {
									close();
								}
							});
						});
					} }, { "Cancel" : function oncancel(inputs, close) {
						close();
					} }], function onstart(inputs) {
						inputs.rpg_character_rights = user_rights(inputs.permission);
					}, function onclose(inputs) {
						inputs.rpg_character_rights.destroy();
					}
			);
			return function (gallery_id) {
				_gallery_id = gallery_id;
				m.show();
			};
		})(); // }}}

		var create_gallery_div = function (item) { // {{{
			var ret = construct.create("div", {
				// id : "gallery-" + item.id,
				"class" : "gallery"
			});
			var inner_wrapper = construct.create("div", { "class" : "gallery-info" , "data-id" : item.id }, ret, "last");
			// for better event delegation, use two sections - for gallery information, and the image collection.
			if (item.image_url) {
				construct.create("img", {
					src : imgurlconverter.small(item.image_url),
					alt : item.title

				}, inner_wrapper);
			}
			on(inner_wrapper, ".delete:click", function(e) { // {{{ button events
				gallery_delete_modal(parseInt(attr.get(this, "data-id")));
				e.stopPropagation();
				e.preventDefault();
			});
			on(inner_wrapper, ".edit:click", function(e) {
				gallery_modify_modal(parseInt(attr.get(this, "data-id")));
				e.stopPropagation();
				e.preventDefault();
			});
			on(inner_wrapper, ".add:click", function(e) {
				image_create_modal(parseInt(attr.get(this, "data-id")));
				e.stopPropagation();
				e.preventDefault();
			}); // }}}

			// var image_wrapper = construct.create("div", { "class" : "image-collection" }, ret, "last");
			// this will be accessed via nextElementSibling
			var image_wrapper = construct.place("<div class=\"imgslider\"></div>", ret);

			construct.place(construct.toDom("<h2 class=\"title\">" + item.title + "</h2>"), inner_wrapper);
			construct.place(construct.toDom("<div class=\"content\"><p>" + item.content + "</p></div>"), inner_wrapper);
			// FUTURE maybe a lot of event listeners will be created. Maybe transform it to delegates.
			btns(inner_wrapper, [ {
				attributes : { "id" : item.id },
				"class" : "edit",
				label : "Edit Gallery",
				permission_type : "edit",
				permission : item.permission
			},{
				attributes : { "id" : item.id },
				"class" : "delete",
				label : "Delete Gallery",
				permission_type : "delete",
				permission : item.permission
			}]);
			btns(inner_wrapper, [ {
				attributes : { "id" : item.id },
				"class" : "add",
				label : "Add picture",
				permission_type : "add",
				permission : item.permission
			}]);
				 
			return ret;
		}; // }}}


	return function (container_id, method_prefix) {
		var nodes = {
			list : null,
			slider : imgslider(method_prefix),
			container : dom.byId(container_id),
			btns : construct.create("div", null, container_id, "first")
		};
		console.log("[gallery_pane] Created gallery for " + method_prefix);

		nodes.list = construct.create("div", null, nodes.container, "last");

		on(nodes.container, ".toc div:click", function (e) {
			var h = hash();
			var prefix = "/";
			if (h.startsWith("/char/")) {
				var idx = h.indexOf("/", 6);
				if (idx == -1) { prefix = h + "/"; }
				else { prefix = h.substr(0, idx); }
			}
			router.go(prefix + "gallery/" + attr.get(this, "data-a"));
		});

		router.register(/\/gallery\/(\w+)/, function (arg) {
			// this method will also be called during expanding
			var q = query(".gallery-info", nodes.container);
			console.log("[gallery_panel] query of all galleries in here.", q);
		});

		on(nodes.container, ".gallery-info:click", function(e) {
			if (dom_class.contains(this, "expanded")) {
				dom_class.remove(this, "expanded");
				nodes.slider.destroy();
			} else {
				var gallery_id = parseInt(attr.get(this, "data-id"));
				var gallery_hash = attr.get(this.parentElement.parentElement, "data-hash-prefix") + gallery_id;
				router.go(gallery_hash); // make store doing an update
				dom_class.add(this, "expanded");

				// safe for img_slider;
				var self = this;
				nodes.slider.show(this.nextElementSibling, gallery_id, function () { dom_class.remove(self, "expanded"); });

				// take over prefix of parent
				attr.set(this, "data-hash-prefix", gallery_hash + "/");
			}
			// TODO register keyboard-steuerung
		});

		return (function () {
			var destroy_list = function () {};
			var keep_track = -1;
			return function update(observable, hash_prefix, keep_track_new, permission) {
				console.log("[gallery_pane] Update gallery for " + method_prefix + " and keep_track_new " + keep_track_new);
				
				// update button-permission
				construct.empty(nodes.btns);
				btns(nodes.btns, [ {
					label : "Create Gallery",
					permission_type : "add",
					permission : permission,
					onclick : function () { gallery_create_modal(method_prefix); }
				}]);

				if (keep_track_new == keep_track) {
					return;
				}

				keep_track = keep_track_new;
				attr.set(nodes.list, "data-hash-prefix", hash_prefix + "/gallery/");
				destroy_list();
				destroy_list = list (nodes.list, observable, create_gallery_div);
			};
		})();
	};
});
