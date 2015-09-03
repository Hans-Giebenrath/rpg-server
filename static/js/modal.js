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
		// "dojo/dom-style",
		"dojo/dom-class",
		"dojo/_base/lang",
		"dojo/topic",
		"dojo/on",
		"rpg/rndm",
		"rpg/overlay",
		"dojo/domReady!"
], function(dom, construct, dom_class, lang, topic, on, rndm, overlay) {
	// TODO move nitification bar to modal and back
	// TODO in a edit-modal, there should be a notification, if the underlying 
	// > object was target to a modification. So one has to listen on incoming stuff.
	// TODO use internationalization. As the keys are almost the same, just prepend an
	// > according span.
	var labels = {
		"char_name" : "Character Name",
		"content" : "Content",
		"delete" : "Delete",
		"email" : "Email",
		"name" : "Name",
		"password" : "Password",
		"permission" : "Permission",
		"role" : "Role",
		"setcover" : "Set as gallery cover",
		"short" : "Short description",
		"slug" : "Slug",
		"tags" : "Tags",
		"title" : "Title",
		"transfer" : "Transfer Ownership",
	};

	// title: str or function returning str, when show occurs.
	// opts. 
	var ret = function(title, opts, objs, btns, onshow, onclose) {
		objs = objs || [];
		opts = lang.mixin({ desc : null, "class" : "", noLabels : false}, opts);
		var node = construct.toDom("<form class=\"" + opts["class"] + "\"><div class=\"header\">" + title + "</div></form>")
		var modal = {};
		var flat = {};

		overlay.init(node, function () {
			if (typeof onshow === "function") onshow(flat);
			if (typeof title === "function") modal.title(title());
		}, function () {
			if (typeof onclose === "function") onclose(flat);
		});

		if (opts["desc"]) {
			var _desc = construct.toDom("<div class=\"description\" >" + opts["desc"] + "</div>");
			construct.place(_desc, node, "last");
		}
		var cont = construct.create("div", {"class" : "content"}, node, "last");
		objs.forEach(function (item) {
			var _id = rndm();
			var _c = construct.create("div", { "class" : "entry leftright" }, cont, "last");
			var _k = Object.keys(item)[0]; //should have just one element
			if (!opts.noLabels) {
				construct.place(construct.toDom("<label for=\"" + _id + "\">" + (item.label || labels[_k] || _k)  + "</label>"), _c, "first");
			}
			construct.place(item[_k], _c, "last");
			item[_k].id = _id;
			flat[_k] = item[_k];
		});

		modal["show"] = function() { 
			overlay.show(node);
		};
		modal["closeOthers"] = function() {
			overlay.closeOthers(node);
		};
		modal["close"] = function() {
			overlay.close(node);
		};
		modal["title"] = function(title) {
			node.firstElementChild.innerHTML = title;
		};
		//
		var btncont = construct.create("div", { "class" : "btn-container" }, node, "last");
		btns.forEach(function(item) {
			var _btn = construct.create("input", {
				type : "button",
				value : Object.keys(item)[0]
			}, btncont, "last");
			on(_btn, "click", function() {
				item[Object.keys(item)[0]](flat, function() {
					modal.close();
					// possibly destroyes in "close"
					if (flat) Object.keys(flat).forEach(function (key) { if (flat[key].value) { flat[key].value = ""; } });
				});
			});
		});

		return modal;
	};

	return ret;
});
