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
define(["dojo/dom-class",
	    "dojo/topic",
		"dojo/hash",
	    "dojo/router",
	    "dojo/domReady!"
], function(dom_class, topic, hash, router) {
	var safeRouterChange = function (to, login_bypassed) {
		if (to === hash() && to !== "") {
			hash("", true);
			console.log("set back hash to retrigger fireing."); //maybe ... ?
		}
		if (login_bypassed) {
			console.log("login bypassed, ", hash());
			router.startup(to);
			return;
		}

		hash(to);
		router.startup();
		return;
	};

	return {
		user_id : -1,
		character_id : -1,
		is_user : false,
		is_gm : false,
		is_char : false,
		group_id : -1,
		group_gallery : -1,
		group_name : "",
		set_none : function() {
			this.user_id = -1;
			this.group_id = -1;
			this.character_id = -1;
			this.is_user = false;
			this.is_gm = false;
			this.is_char = false;
			this.group_gallery = -1;
			this.group_name = "";
			topic.publish("logout");
		},
		set_user : function(result) {
			if (result && result.user_id) this.user_id = parseInt(result.user_id);
			assert (this.user_id > 0, "User_Id is not set properly");
			this.group_id = -1;
			this.character_id = -1;
			this.is_user = true;
			this.is_gm = false;
			this.is_char = false;
			dom_class.add("add-char", "hidden");
			dom_class.add("add-gm", "hidden");
			topic.publish("enter_user", result);
		},
		set_gm : function(result, group_id) {
			assert (this.user_id > 0, "User_Id is not set properly");
			result.old_group_id = this.group_id;
			this.group_id = parseInt(group_id) || parseInt(result.group_id);
			assert (this.group_id > 0, "Group_Id is not set properly");
			this.character_id = -1;
			this.is_user = false;
			this.is_gm = true;
			this.is_char = false;
			this.group_name = result.group.info[0];
			this.group_gallery = parseInt(result.group.info[1]);
			dom_class.remove("add-char", "hidden");
			dom_class.remove("add-gm", "hidden");
			topic.publish("enter_group", result);

			var login_bypassed = group_id === true;
			safeRouterChange("/history", login_bypassed);
		},
		set_char : function(result, group_id, character_id) {
			assert (this.user_id > 0, "User_Id is not set properly");
			result.old_group_id = this.group_id;
			this.group_id = parseInt(group_id) || parseInt(result.group_id);
			this.character_id = parseInt(character_id) || parseInt(result.character_id);
			assert (this.group_id > 0, "Group_Id is not set properly");
			assert (this.character_id > 0, "Character_Id is not set properly");
			this.is_user = false;
			this.is_gm = false;
			this.is_char = true;
			this.group_name = result.group.info[0];
			this.group_gallery = parseInt(result.group.info[1]);
			dom_class.add("add-char", "hidden");
			dom_class.add("add-gm", "hidden");
			topic.publish("enter_group", result);

			var login_bypassed = group_id === true;
			safeRouterChange("/char/" + this.character_id, login_bypassed);
		},
	};
});

