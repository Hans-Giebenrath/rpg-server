"use strict"
require(["dojo/dom",
		 "dojo/dom-construct",
		 "dojo/dom-class",
		 "dojo/dom-attr",
		 "dojo/on",
		 "dojo/topic",
		 "rpg/list",
		 "rpg/modal",
		 "rpg/soul",
		 "rpg/ajax",
		 "rpg/store"
], function(dom, construct, dom_class, attr, on, topic, list, modal, soul, ajax, store) {
	var nodes = {
		container : dom.byId("appointments"),
		new_date : dom.byId("appointment-date"),
		propose : dom.byId("appointment-propose"),
		clear : dom.byId("appointment-clear")
	};

	var insertRow = function(item) {
		var usernames = item.electors.map(function(user_id) { // TODO map from ids to usernames
			return store.user.get(user_id).name;
		});
		var ret = construct.toDom("<div class=\"entry leftright\"><span class=\"date tooltip\" data-tooltip=\"" + usernames.join(", ") + "\">" + item.date + " (" + usernames.length + ")</span></div>");
		if (item.electors.indexOf(soul.user_id) > -1) {
			// unelect
			construct.create("input", { value: "Unvote", type : "button", "data-appointment" : item.id, "data-kind" : "unvote", "class" : "unvote"}, ret, "last");
			dom_class.add(ret, "voted");
		} else {
			// elect
			construct.create("input", { value: "Vote", type : "button", "data-appointment" : item.id, "data-kind" : "vote", "class" : "vote"}, ret, "last");
		}

		if (item.elected) {
			if (soul.character_id == -1) {
				construct.create("input", { value: "Unelect", type : "button", "data-appointment" : item.id, "data-kind" : "unelect", "class" : "unelect"}, ret, "last");
			}
			dom_class.add(ret, "elected");
		} else {
			if (soul.character_id == -1) {
				construct.create("input", { value: "Elect", type : "button", "data-appointment" : item.id, "data-kind" : "elect", "class" : "elect"}, ret, "last");
			}
		}
		return ret;
	};

	on(nodes.container, "input:click", function(e) {
		var kind = attr.get(this, "data-kind");
		var id = attr.get(this, "data-appointment");
		switch(kind) {
			case "unvote" :
				ajax("appointment_edit", [id, 0]);
				break;
			case "vote" : 
				ajax("appointment_edit", [id, 1]);
				break;
			case "unelect" :
				ajax("appointment_edit", [id, 2]);
				break;
			case "elect" :
				ajax("appointment_edit", [id, 3]);
				break;
		}
	});

	on(nodes.propose, "click", function(e) {
		var new_date = nodes.new_date.value;
		if (new_date == "") {
			alert("First insert some date, Mylord");
			return;
		}
		ajax("appointment_create", [new_date], function () { nodes.new_date.value = ""; });
	});

	var appointment_clear_modal = modal("Please confirm", {desc: "Do you really want to delete all appointments?", "class" : "small"}, null,
										 [{"Yes, clear all dates." : function(inputs, close) { ajax("appointment_delete", [], close); }}, {"Oups, don't clear." : function (inputs, close) { close(); }}]);
	on(nodes.clear, "click", function(e) {
		appointment_clear_modal.show();
	});

	// NOTE only one time creation necessary, as it reacts correctly to insertions and deletions
	/* var destroy_list =*/ list(nodes.container, store.appointment.query(), insertRow);

	topic.subscribe("enter_group", function() {
		if (soul.character_id !== -1) {
			dom_class.add(nodes.clear, "hidden");
		} else {
			dom_class.remove(nodes.clear, "hidden");
		}
	});
});
