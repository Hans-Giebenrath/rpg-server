"use strict"
define(["dojo/dom-construct",
	   "dojo/on",
	   "dojo/dom-class",
	   "dojo/_base/lang",
	   "rpg/permission_mapping",
	   "rpg/soul"
], function (construct, on, dom_class, lang, permission_mapping, soul) {
	// 1. container
	// 2. information, that till be bound to the dom
	// 3. what shall happen on edit?
	// 4. what shall happen on delete?
	// 5. displayed label
	// TODO query on permission broker, or whatsoever. Maybe there is a permission change.
	// TODO do not insert a disabled button, but a hidden div. Then this can be exchanged.

	var create = function (container, _class, attributes, onclick, label, permission_type, permission) {
		var a = {};
		// if there is no permission stuff specified, it surely is ok.
		permission = parseInt(permission) || permission || 0;
		if (soul.character_id !== -1 && permission < permission_mapping[permission_type].value) {
			return;
			// a["disabled"] = "disabled";
		}

		if (!attributes) {
			attributes = {};
		}

		Object.keys(attributes).forEach(function (key) {
			// only extend with real strings and stuff. Also, permission is never useful.
			if (key !== "permission" && typeof attributes[key] == "string" || typeof attributes[key] == "number") {
				a["data-" + key] = attributes[key];
			}
		});
		var btn = construct.create("input", lang.mixin({
			type : "button",
			"class" : _class,
			value : label
		}, a), container, "last");

		if (typeof onclick == "function") {
			on(btn, "click", onclick);
		}

	};

	var create_inner_cont = function (container) {
		return construct.create("div", {"class" : "btn-container"}, container, "last");
	};

	/* elements = {
	*    class : "some class stuff"
	*    data : {}
	*    onclick : function () {}
	*    label : ""
	*    permission_type : "edit|add|..."
	*    permission : store.permission
	*/
	return function (container, elements) { //types, information, label, onclickhandler) {
		var inner_cont = create_inner_cont(container);
		elements.forEach(function (item) {
			create(inner_cont, item["class"] || "", item.attributes, item.onclick, item.label, item.permission_type, item.permission);
		});
	};
});
