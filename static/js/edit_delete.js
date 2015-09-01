"use strict"
define(["dojo/dom-construct",
	   "dojo/on",
	   "rpg/permission_mapping"
], function (construct, on, permission_mapping) {
	// 1. container
	// 2. information, that till be bound to the dom
	// 3. what shall happen on edit?
	// 4. what shall happen on delete?
	// 5. displayed label

	var create = {
		edit : function (container, information, onedit, label) {
			if (information.permission >= permission_mapping.edit.value) {
				var btn = construct.create("input", {
					type : "button",
					"class" : "edit",
					value : label
				}, inner_cont, "last");
				btn.information = information;
				if (typeof onedit == "function") {
					on(btn, "click", onedit);
				}
			}
		}, 

		add : function (container, information, onadd, label) {
			if (information.permission >= permission_mapping.edit.value) {
				var btn = construct.create("input", {
					type : "button",
					"class" : "add",
					value : label
				}, inner_cont, "last");
				btn.information = information;
				if (typeof onadd == "function") {
					on(btn, "click", onadd);
				}
			}
		},

		"delete" : function (container, information, ondelete, label) {
			if (information.permission >= permission_mapping["delete"].value) {
				var btn = construct.create("input", {
					type : "button",
					"class" : "delete",
					value : label
				}, inner_cont, "last");
				btn.information = information;
				if (typeof ondelete == "function") {
					on(btn, "click", ondelete);
				}
			}
		}
	};

	var create_inner_cont = function (container) {
		return construct.create("div", null, container, "last");
	};

	return function (types, container, information, onclickhandler, label) {
		var inner_cont = create_inner_cont(container);
		for (var i = 0; i < types.length; i++) {
			create[types[i]](inner_cont, information, onclickhandler[i], label[i]);
		}
	};
});
