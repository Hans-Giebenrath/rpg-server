"use strict"
define(["dojo/dom-construct",
		"dojo/dom-class",
		"dojo/dom-attr",
		"dojo/on",
		"dojo/query!css3",
	    "rpg/list",
	    "rpg/store"
], function (construct, dom_class, attr, on, query, list, store) {
	var create_tag = function (object) {
		var ret = construct.toDom("<span class=\"tag\" data-tag=\"" + object.tag + "\" data-id=\"" + object.id + "\" >" + object.tag + "</span>");
		return ret;
	};

	return function (container, onchange, extra) {
		// onchange - simple event callback
		// extra : show extra input field
		var lc = construct.create("div", { "class" : "tag"}, container, "first");
		var list_destroy = list(lc, store.tag.query(), create_tag, function (item) {
			onchange();
		});
		var extra_input;
		var extra_input_timeout;
		if (extra) {
			extra_input = construct.toDom("<input type=\"text\" placeholder=\"Create tags\" />");
			construct.place(extra_input, container, "last");
			["keydown", "cut", "paste"].forEach(function(evt) {
				on(extra_input, evt, function (e) {
					if (extra_input_timeout) window.clearTimeout(extra_input_timeout);
					extra_input_timeout = window.setTimeout(onchange, 400);
				});
			});
		}
		on(lc, "span:click", function(e) {
			dom_class.toggle(this, "selected");
			if (typeof onchange == "function") onchange();
		});
		var obj = {
			destroy : function () { list_destroy(); construct.empty(container); },
			list : function () {
				var lt = {};
				query("span.selected", lc).forEach(function(item) { lt[attr.get(item, "data-tag")] = true; });
				if (extra) {
					var matches = extra_input.value.match(/\w+/g);
					if (matches) { matches.forEach(function(item) { lt[item] = true; }); }
				}
				return Object.keys(lt);
			},
			list_id : function () {
				var lt = {};
				query("span.selected", lc).forEach(function(item) { lt[attr.get(item, "data-id")] = true; });
				return Object.keys(lt).map(function(k) {return parseInt(k);});
			},
			list_sql : function () {
				return "{" + this.list().join(",") + "}";
			},
			preselect : function (tags) {
				query("> *", lc).forEach(function(item) {
					if (tags.some(function(tag) { return attr.get(item, "data-tag") == tag.tag; })) {
						dom_class.add(item, "selected");
					}
				});
			},
			preselect_all : function () {				
				query("> *", lc).forEach(function(item) {
					dom_class.add(item, "selected");
				});
			}
		};
		return obj;
	}
});
