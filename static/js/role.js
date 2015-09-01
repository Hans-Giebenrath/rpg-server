"use strict"
define(["dojo/dom",
	    "dojo/dom-construct",
		"dojo/dom-attr",
		"dojo/dom-class",
	    "dojo/query",
	    "dojo/request",
		"dojo/cookie",
		"dojo/hash",
		"dojo/on",
		"rpg/log",
		"rpg/soul",
		"rpg/tabpane",
		"rpg/modal",
		"rpg/list",
		"rpg/store",
		"rpg/ajax",
		"dojo/NodeList-dom",
		"dojo/domReady!"
], function(dom, construct, attr, dom_class, query, request, cookie, hash, on, log, soul, tabpane, modal, list, store, ajax) {
// own_characters_list (character_id, character_name, character_short, group_id, group_name)
	var login_modal;
	var role_modal;
	var register_done_modal;
	var delete_group_modal;
	var nodes = {
		own_chars : dom.byId("own-characters-list"),
		own_gms : dom.byId("own-gms-list"),
		last_active_role : null
	};

	var self;

	register_done_modal = modal(
		"Registration successful.",
		{ desc: "Please wait, until the admin unlocked you. Actually, inform someone who knows the admin.", "class" : "small"} ,
		[], [{"Okai, I inform him" : function(inputs, close) { close(); }}]
	);
	login_modal = modal( // {{{
		"Hello, stranger ...",
		{ "class" : "small" },
		[
			{
				name : construct.create(
					"input", {
						type : "text",
						placeholder : "Name"
				})},{
				password : construct.create(
					"input", {
						type : "password",
						placeholder : "Password"
				})},{
				email : construct.create(
					"input", {
						type : "email",
						placeholder : "E-Mail (for registration)"
				})}
		],
		[
			{"Login" : function(inputs, close) {
				self.login(
					inputs.name.value,
					inputs.password.value,
					function () {
						role_modal.show();
						close();
					}
				);
				
			}},
			{"Register" : function(inputs, close) {
				self.register(
					inputs.name.value,
					inputs.password.value,
					inputs.email.value,
					register_done_modal.show
				);

			}}
		], null, function(inputs) {
		}
	); // }}}
	// this gets closed as success-handler of all-my-roles .selectable:click
	role_modal = modal( // {{{
		"Which role you want to cover in?",
		{"class" : "small", noLabels : true},
		[{role : construct.create("ul")}],
		[ { "Logout" : function(inputs, close) {
			self.logout();
		}}],
		function (inputs) {
			construct.place("all-my-roles", inputs.role, "first");
		},
		function (inputs) {
			construct.place("all-my-roles", dom.byId("all-my-roles-cb"), "after");
		}
	); // }}}

	delete_group_modal = (function() { // {{{
		var group_id;
		var m = modal(
			"Delete a group",
			{ desc : "Are you sure, that you want to delete this group? Every text and picture will be deleted. There is no restore feature.", "class" : "small"},
			[{ div : construct.toDom("<span></span>"), label : "Chosen group"}],
			[{"Delete group" : function(inputs, close) {
				ajax("group_delete", [group_id], close);
			}}, {"Cancel" : function(inputs, close) {
				close();
			}}], function onshow(inputs) {
				inputs.div.textContent = store.own_gms.get(group_id).name;
			}
		);

		return function (_group_id) {
			group_id = _group_id;
			m.show();
		};
	})(); // }}}

	on(dom.byId("all-my-roles"), ".selectable:click", function(e) { // {{{
		if (attr.get(this, "data-rpg-role-type") === "char") {
			self.change_char(
				attr.get(this, "data-rpg-group-id"),
				attr.get(this, "data-rpg-character-id"),
				role_modal.close // onsuccess
			);
		} else {
			self.change_gm(
				attr.get(this, "data-rpg-group-id"),
				role_modal.close // onsuccess
			);
		}
	}); // }}}

	on(nodes.own_gms, ".delete:click", function(e) {
		e.preventDefault();
		e.stopPropagation();
		delete_group_modal(parseInt(attr.get(this, "data-id")));
	});

	var set_selected = function(el) {
		if (el !== null && el !== undefined) {
			dom_class.add(el, "selected");
			dom_class.remove(el, "selectable");
		}
		var last = nodes.last_active_role;
		if (last !== null) {
			dom_class.add(last, "selectable");
			dom_class.remove(last, "selected");
		}
		nodes.last_active_role = el;
	};

	/* var own_chars_list =*/
	list(nodes.own_chars, store.own_chars.query(), function(item) {
		// as modal dialog has to be closed, is is not practical do add
		// event listener at this place.
		var ret = construct.create("div", {
			"data-rpg-role-type" : "char",
			"data-rpg-character-id" : item.id,
			"data-rpg-group-id" : item.group_id,
			"class" : "selectable lvl3"
		});
		item.setSelected = function() {set_selected(ret)};
		if (item.id == soul.character_id) { item.setSelected(); }
		// don't fear XSS
		ret.innerHTML = "<span>" + item.name + "</span><span class=\"info\">" + item.group_name + "</span>";
		return ret;
	});

	/* var own_gms_list =*/
	list(nodes.own_gms, store.own_gms.query(), function(item) {
		var ret = construct.create("div", {
			"data-rpg-role-type" : "gm",
			"data-rpg-group-id" : item.id,
			"class" : "selectable lvl3"
		});
		item.setSelected = function() {set_selected(ret)};
		if (item.id == soul.group_id && soul.is_gm) { item.setSelected(); }
		// don't fear XSS
		ret.innerHTML = "<span>" + item.name + "</span><input type=\"button\" value=\"Delete\" class=\"delete\" data-id=\"" + item.id + "\" />";
		return ret;
	});

	request("/status/ids", {
		handleAs : "json",
		preventCache : true
	}).then(function (result) {
		if (result.data) result = result.data;
		if (!!result.user_id) {
			soul.set_user(result);
			if (result.group_id !== "") {
				if (result.character_id !== "") {
					soul.set_char(result, true /* will display the hash */);
					store.own_chars.get(result.character_id).setSelected();
				} else {
					soul.set_gm(result, true);
					store.own_gms.get(result.group_id).setSelected();
				}
			} else {
				self.choose_role();
			}
		} else {
			login_modal.show();
			hash("", true);
		}

		console.log("dosijdosijd", result);
	}, function (err) {
		log.error(err.response.data, err);
	});

	// login_modal.show();

	on(dom.byId("logout"), "click", function(e) {
		self.logout();
	});

	on(dom.byId("create-group"), "click", function(e) {
		self.group_create(dom.byId("new-group-name").value, function() {
			dom.byId("new-group-name").value = "";
			role_modal.close();
		});
	});

	var user_delete_modal = modal(
		"Delete your account", {desc : "Are you sure, you want to delete your account, including all groups, where you are the only gm? This will not delete you characters in others groups. They will be orphaned.", "class" : "small"},
		[], [{ "Delete my account." : function(inputs, close) {
			ajax("user_delete", [], close);
		}}, { "No! Bad idea." : function(inputs, close) { close(); }}]);
	on(dom.byId("user-delete"), "click", function(e) {
		user_delete_modal.show();
	});

	var user_edit_modal = modal(
		"Modify your account", { desc : "For changing your account, you have to provide your current password. Leave the \"new password\" field empty to keep the current one. Same applies to your email." },
		[
			{ name : construct.toDom("<input type=\"text\" />"), label : "Your name"},
			{ old_pw : construct.toDom("<input type=\"password\" placeholder=\"Current Password\" />"), label : "Current PW"},
			{ email : construct.toDom("<input type=\"email\" placeholder=\"E-Mail\" />"), label : "E-Mail"},
			{ new_pw : construct.toDom("<input type=\"password\" placeholder=\"New Password\" />"), label : "New PW"},
			{ new_rep_pw : construct.toDom("<input type=\"password\" placeholder=\"New Password (again)\" />"), label : "New PW (again)"}
		], [
			{ "Modify account" : function(inputs, close) {
			if (inputs.new_pw.value && inputs.new_pw.value !== inputs.new_rep_pw.value) {
				log.info("Your new password does not match the repetition.");
				return
			}

			ajax("user_edit", [inputs.old_pw.value, inputs.name.value, inputs.new_pw.value, inputs.email.value], close);
		}} , { "Don't modify" : function (inputs, close) { close(); } }
		], function onshow(inputs) {
			inputs.name.value = store.user.get(soul.user_id).name;
		}
	);
	on(dom.byId("user-edit"), "click", function(e) {
		user_edit_modal.show();
	});

	// Duplicate? WTF?!
	/* var user_delete_modal = modal(
		"Delete your account", {desc : "Are you sure, you want to delete your account, including all groups, where you are the only gm? This will not delete you characters in others groups. They will be orphaned.", "class" : "small"},
		[], [{ "Delete my account." : function(inputs, close) {
			ajax("user_delete", [], close);
		}}, { "No! Bad idea." : function(inputs, close) { close(); }}]);
	on(dom.byId("user-delete"), "click", function(e) {
		user_delete_modal.show();
	});*/

	self = {
		change_char : function(group_id, character_id, success, error) {
			if (character_id == soul.character_id && group_id == soul.group_id) return;

			request("/role/check_role_character", {
				query : {
					group_id : group_id,
					character_id : character_id
				},
				handleAs : "json",
				preventCache : true
			}).then(function (result) {
				tabpane.prepare("char");
				// required for ws.js to check,
				// whether it is an "enter_group" or "switch_group" event.
				soul.set_char(result, group_id, character_id);
				store.own_chars.get(character_id).setSelected();

				if (typeof success === "function") success();
			}, function (err) {
				log.error(err.response.data, err);
				if (typeof error === "function") error(err);
			});
		},
		change_gm : function(group_id, success, error) {
			request("/role/check_role_gm", {
				query : {
					group_id : group_id
				},
				handleAs : "json",
				preventCache : true
			}).then(function (result) {
				tabpane.prepare("history");
				// required for ws.js to check,
				// whether it is an "enter_group" or "switch_group" event.
				soul.set_gm(result, group_id);
				store.own_gms.get(group_id).setSelected();

				if (typeof success === "function") success();
			}, function (err) {
				log.error(err.response.data, err);
				if (typeof error === "function") error(err);
			});
		},
		group_create : function(group_name, success, error) {
			ajax("group_create", [group_name], function (result) {
				store.own_gms.rpg_insert([[result[0], group_name, result[1]]]);

				// NOTE this matches only a single record.
				query("#own-gms-list [data-rpg-role-type=\"gm\"][data-rpg-group-id=\"" + result[0] + "\"]")
					.addClass("selected")
					.removeClass("selectable")
					.forEach(function(node) { node.id = "last-active-role"; });
				var last_active = dom.byId("last-active-role");
				if (last_active !== null && last_active !== undefined) {
					last_active.id = "";
					dom_class.add(last_active, "selectable");
					dom_class.remove(last_active, "selected");
				}
				// clear the previous hash, so that we will go straight to the start
				// page (history?)
				hash("", true);
				self.change_gm(result[0], success, error);
				// there should be no error. otherwise the hell broke up
			}, error);
		},
		close : function () {
			// sadly (or not), this won't work, as one is only allowed to close
			// a window, which was opened by the script.
			// window.close();
			// So, simply logout instead
			this.logout();
		},
		logout : function(close) {
			soul.set_none();
			// role_modal.close();
			login_modal.show();
			login_modal.closeOthers();
			cookie("AWS", null, {expires: -1})
		},
		choose_role : function() {
			soul.set_user();

			// deselect the previous one / clear last_active_role
			set_selected(null);

			// login_modal.close();
			role_modal.show();
			role_modal.closeOthers();
		},
		login : function(name, password, success, error) {
			request("/role/login", {
				query : {
					name : name,
					password : password
				},
				handleAs : "json",
				preventCache : true
			}).then(function (result) {
				soul.set_user(result);
				if (success !== undefined) success();
			}, function (err) {
				log.error(err.response.data, err);
				if (error !== undefined) error();
			});
		},
		register : function(name, password, email, success, error) {
			request("/role/register", {
				query : {
					name : name,
					password : password,
					email : email
				},
				handleAs : "json",
				preventCache : true
			}).then(function (result) {
				if (success !== undefined) success();
			}, function (err) {
				log.error(err.response.data, err);
				if (error !== undefined) error();
			});
		}
	};

	return self;
});
