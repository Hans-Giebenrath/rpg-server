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
		"dojo/domReady!"
], function(dom, construct, dom_class, lang, topic, on, rndm) {
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

	var currently_shown = 0;
	var all_modals = [];
	var overlay = dom.byId("modal");
	var content = dom.byId("content");
	var close_modal = function(el) {
		currently_shown--;
		construct.place(el, "hidden-zone");
		if (currently_shown === 0) {
			dom_class.add(overlay, "hidden");
			dom_class.remove(content, "hidden");
		} else {
			dom_class.remove(overlay.firstElementChild, "hidden");
		}
	};
	
	// this is required for immediate login,
	// when the modals are skipped.
	topic.subscribe("enter_user", function () {
		if (currently_shown === 0) {
			dom_class.add(overlay, "hidden");
			dom_class.remove(content, "hidden");
		}
	});
	topic.subscribe("enter_group", function () {
		if (currently_shown === 0) {
			dom_class.add(overlay, "hidden");
			dom_class.remove(content, "hidden");
		}
	});

	var show_modal = function(el)	{
		currently_shown++;
		dom_class.remove(el, "hidden"); //however this can happen ...
		if (overlay.firstElementChild != null) {
			dom_class.add(overlay.firstElementChild, "hidden");
		} else {
			dom_class.remove(overlay, "hidden");
			dom_class.add(content, "hidden");
		}
		construct.place(el, overlay, "first");
	};

	var ret = function(title, opts, objs, btns, onshow, onclose) {
		objs = objs || [];
		opts = lang.mixin({ desc : null, "class" : null, noLabels : false}, opts);
		var obj = {
			node : construct.toDom("<form><div class=\"header\">" + title + "</div></form>"),
			state : "close"
		};
		all_modals.push(obj);
		if (opts["class"]) { dom_class.add(obj.node, opts["class"]); }
		construct.place(obj.node, "hidden-zone");
		if (opts["desc"]) {
			var _desc = construct.toDom("<div class=\"description\" >" + opts["desc"] + "</div>");
			construct.place(_desc, obj.node, "last");
		}
		var cont = construct.create("div", {"class" : "content"}, obj.node, "last");
		var flat = {};
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

		obj["show"] = function(cl) { 
			if (obj.state == "show") return;
			obj.state = "show";
			if (typeof onshow === "function") onshow(flat);
			show_modal(obj.node);
		};
		obj["closeOthers"] = function() {
			all_modals.forEach(function(el) {
				if (el === obj) return; // continue

				el.close();
			});
		};
		obj["close"] = function() {
			if (obj.state == "close") return;
			obj.state = "close";
			close_modal(obj.node);
			if (typeof onclose === "function") onclose(flat);
		};
		obj["destroy"] = function() {
			construct.destroy(obj.node);
			onshow = null;
			onclose = null;
			btns = null;
			objs = null;
			opts = null;
			title = null;
			flat = null;
			obj.show = null;
			obj.close = null;
			obj.destroy = null;
			obj = null;
		};
		obj["title"] = function(title) {
			obj.node.firstElementChild.innerHTML = title;
		};
		//
		var btncont = construct.create("div", { "class" : "btn-container" }, obj.node, "last");
		btns.forEach(function(item) {
			var _btn = construct.create("input", {
				type : "button",
				value : Object.keys(item)[0]
			}, btncont, "last");
			on(_btn, "click", function() {
				item[Object.keys(item)[0]](flat, function() {
					obj.close();
					// possibly destroyes in "close"
					if (flat) Object.keys(flat).forEach(function (key) { if (flat[key].value) { flat[key].value = ""; } });
				});
			});
		});

		return obj;
	};

	return ret;
});
