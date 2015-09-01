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

/*<div id="slider"> {{{
<input type="radio" name="lol" id="cb1" checked="checked"/>
<div>
<div class="pic">
<label class="left" for="cb3">&lt;</label>
<div class="bg">
<img src="https://www.samhober.com/howtofoldpocketsquares/Flat1.jpg" />
</div>
<label class="right" for="cb2">&gt;</label>
</div>
<div class="title">
Some 111!
</div>
<div class="content">
Lorem 111 dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
	</div>
</div>
<input type="radio" name="lol" id="cb2"/>
<div>
<div class="pic">
<label class="left" for="cb1">&lt;</label>
<div class="bg">
<img src="https://www.samhober.com/howtofoldpocketsquares/Flatsuit2.jpg" />
</div>
<label class="right" for="cb3">&gt;</label>
</div>
<div class="title">
Some 222!
</div>
<div class="content">
Lorem 222 dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
	</div>
</div>
<input type="radio" name="lol" id="cb3"/>
<div>
<div class="pic">
<label class="left" for="cb2">&lt;</label>
<div class="bg">
<img src="https://www.samhober.com/howtofoldpocketsquares/Flat3.jpg" />
</div>
<label class="right" for="cb1">&gt;</label>
</div>
<div class="title">
Some 333!
</div>
<div class="content">
Lorem 333 dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
	</div>
</div>
</div>
<div>
PPPPP
</div>*/ // }}}

"use strict"
/**
* TODO it is somewhat buggy. The creation of IDs doesn't work properly, so radio-boxes have in ID with [Object object] as substring. Also, there is always an exception on startup.
*/
define(["dojo/dom",
	    "dojo/dom-construct",
		"dojo/dom-class",
		"dojo/dom-attr",
		"dojo/debounce",
		"dojo/query",
		"dojo/on",
		"rpg/list",
	    "rpg/ajax",
		"rpg/user_rights",
		"rpg/imgurlconverter",
		"rpg/modal",
		"rpg/markdown",
		"rpg/store",
		"rpg/orderable"
], function (dom, construct, dom_class, attr, debounce, query, on, list, ajax, user_rights, imgurlconverter, modal, markdown, store, orderable) {
	var nodes = {
		root : null,
		orderable : null,
		image_view : null
	}

	var clear_nodes = function() {
		dom_class.remove(nodes.root, "leftright");
		construct.empty(nodes.root);
		nodes.root = null;
		nodes.orderable = null;
		nodes.image_view = null;
	};

	var prepare_nodes = function(container) {
		nodes.root = container;
		nodes.orderable = construct.place("<div></div>", nodes.root);
		nodes.image_view = construct.place("<div class=\"image-collection\"></div>", nodes.root);
		dom_class.add(nodes.root, "leftright");
	}

	var image_modify_modal = (function() { // {{{
		var i;
		var m = modal(
			"Edit Image", null, [
				{ title : construct.toDom("<input type=\"text\" />")},
				{ content : construct.toDom("<textarea />")},
				{ permission : construct.toDom("<div></div>") }], [
				{ "Save" : function oncreategallery(inputs, close) {
					// NOTE i.permission_broker is necessary for automatic push of permissions to clients.
					ajax("image_edit", [i.id, inputs.title.value, markdown.convert(inputs.content.value), inputs.content.value, inputs.rpg_character_rights.get_permissions(), i.permission_broker], close);
				} }, { "Cancel" : function oncancel(inputs, close) {
					close();
				} }], function onstart(inputs) {
					inputs.rpg_character_rights = user_rights(inputs.permission, i.permission_broker, "image", i.id); // TODO
					inputs.title.value = i.title;
					inputs.content.value = i.content_raw;
				}, function onclose(inputs) {
					inputs.rpg_character_rights.destroy();
				}
		);
		return function (image_id) {
			store.image.rpg_get(image_id, function (_i) {
				i = _i;
				// wait, until permissions really are present. Then show the modal.
				store.permission.rpg_get("image", i.id, m.show);
			});
		};
	})(); // }}}
		var image_delete_modal = (function() { // {{{
			var image_id;
			var m = modal(
				"Delete Image", {desc : "Are you sure you want to delete it?", "class" : "small"}, [ ], [
					{ "Delete Image" : function oncreategallery(inputs, close) {
				ajax("image_delete", [image_id], close);
			} }, { "Cancel" : function oncancel(inputs, close) {
				close();
			} }]
			);
			return function (_image_id) {
				image_id = _image_id
				m.show();
			};
		})(); // }}}

	var buildIds = function (slider_id) {
		if (!nodes.image_view) return; // this means, that the list is not displayed
		slider_id = slider_id.id;

		var nl = query("> div", nodes.image_view);
		var i = 0;
		nl.forEach(function (item) {
			// slider-element
			item.children[0].id = "slider-cb-" + slider_id + "-" + i;
			item.children[1].children[0].children[0].htmlFor = "slider-cb-" + slider_id + "-" + (i == 0 ? nl.length - 1 : i - 1);
			item.children[1].children[0].children[2].htmlFor = "slider-cb-" + slider_id + "-" + (i == nl.length - 1 ? 0 : i + 1);
			i++;
		});

		nl = query("label", nodes.orderable);
		i = 0;
		nl.forEach(function (item) {
			item.htmlFor = "slider-cb-" + slider_id + "-" + i;
			i++;
		});
	};
	var insertListElement = function (sliderid) {
		return function (item) {
			var ret = construct.toDom(
				"<div data-id=\"" + item.id + "\" class=\"slider-element\">" + 
					"<input class=\"hidden\" type=\"radio\" name=\"slider-radio-"+ sliderid + "\" checked=\"checked\"/>" +
					"<div>" +
						"<div class=\"pic\">" +
							"<label class=\"left\" >&lt;</label>" +
							"<a href=\"" + imgurlconverter.orig(item.url, item.id) + "\" target=\"_blank\" type=\"image\">" +
								"<div class=\"bg\">" +
									"<img alt=\"buggibuggibuu\" src=\"" + imgurlconverter.medium(item.url) + "\" data-url=\"" + item.url + "\" />" +
								"</div>" +
							"</a>" +
							"<label class=\"right\" >&gt;</label>" +
						"</div>" +
						"<div class=\"image-detail\">" +
							"<h3 class=\"title\">" +
								item.title +
							"</h3>" +
							"<div class=\"content\">" +
								item.content +
							"</div>" +
							"<div class=\"btn-container\">" +
								"<input type=\"button\" value=\"Set As Cover\" class=\"cover\" data-id=\"" + item.id + "\" data-gallery-id=\"" + item.gallery + "\" />" +
								"<input type=\"button\" value=\"Edit Image\" class=\"edit\" data-id=\"" + item.id + "\" data-gallery-id=\"" + item.gallery + "\" />" +
								"<input type=\"button\" value=\"Delete Image\" class=\"delete\" data-id=\"" + item.id + "\" data-gallery-id=\"" + item.gallery + "\" />" +
							"</div>" +
						"</div>" +
					"</div>" +
				"</div>"
			);
			return ret;
		};
	};

	/** Maybe the parameter is useful in the future ... */
	var insertOrderableRow = function (slider_id) {
		return function (item) {
			return construct.toDom(
				"<div class=\"pic\" data-id=\"" + item.id + "\">" +
					"<label>" + 
						"<div class=\"bg\">" +
							"<img alt=\"buggibuggibuu\" src=\"" + imgurlconverter.small(item.url) + "\" data-url=\"" + item.url + "\" />" +
						"</div>" +
					"</label>" +
				"</div>"
			);
		};
	};
	var reorder = function (gallery_id) {
		return function (o) {
			ajax("image_order_edit", [gallery_id, o.getOrder(false /** ascending */)]);
		};
	};

	var ret = {};
	return function (slider_id) {
		var ret = {};
		console.log("[imgslider] Created slider " + slider_id);
		var destroy_list;
		var orderable_handle;
		ret["show"] = function (container, gallery_id, close_container /** function */) {
			if (typeof ret["destroy"] == "function") ret["destroy"]();
			console.log("[imgslider] Show slider " + slider_id + " for gallery " + gallery_id);
			prepare_nodes(container);
			var observable = store.image.query({ gallery : gallery_id }, {sort : [{attribute : "order", descending : false }]});
			orderable_handle = orderable(nodes.orderable, observable, insertOrderableRow(slider_id), debounce(reorder(gallery_id), 800), store.gallery.get(gallery_id).permission);
			destroy_list = list(nodes.image_view, observable, insertListElement(slider_id), buildIds, buildIds, buildIds);
			var signals = [];
			signals.push(
				on(nodes.image_view, ".cover:click", function(e) {
					ajax("gallery_set_image", [attr.get(this, "data-gallery-id"), attr.get(this, "data-id")]);
					e.stopPropagation();
					e.preventDefault();
				})
			);
			signals.push(
				on(nodes.image_view, ".delete:click", function(e) {
					// image_delete_modal(parseInt(attr.get(this, "data-id")));
					image_delete_modal(attr.get(this, "data-id"));
					e.stopPropagation();
					e.preventDefault();
				})
			);
			signals.push(
				on(nodes.image_view, ".edit:click", function(e) {
					// image_modify_modal(parseInt(attr.get(this, "data-id")));
					image_modify_modal(attr.get(this, "data-id"));
					e.stopPropagation();
					e.preventDefault();
				})
			);
			container.scrollIntoView();
			ret["destroy"] = function () {
				container = null;
				destroy_list();
				orderable_handle.destroy();
				close_container(); // this surely exists
				clear_nodes();
				console.log("[imgslider] Close slider " + slider_id + " for gallery " + gallery_id);
				ret["destroy"] = function () {};
				signals.forEach(function(item) { item.remove(); });
			};
		};
		return ret;
	};
});
