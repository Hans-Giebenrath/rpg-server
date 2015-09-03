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
	    "dojo/_base/lang",
	    "dojo/dom-construct",
		"dojo/dom-class",
		"dojo/dom-attr",
	    "dojo/store/Observable",
	    "dojo/store/Memory",
		"dojo/debounce",
		"dojo/on",
		"rpg/list",
	    "rpg/ajax",
		"rpg/overlay",
		"rpg/soul"
], function (dom, lang, construct, dom_class, attr, observable, memory, debounce, on, list, ajax, overlay, soul) {
	var nodes = {
		root : null,
		sheet : null,
		edit : null,
		save : null,
		close : null,
	}
	var states = { disabled : 0, enabled : 1 };
	var state = states.disabled;
	var module = {};

	var character = {
		misc : new observable(new memory({})),
		attributes : new observable(new memory({})),
		savings : new observable(new memory({})),
		fight : new observable(new memory({})),
		attack : new observable(new memory({})),
		skills : new observable(new memory({})),
		feats : new observable(new memory({})),
		gear : new observable(new memory({})),
		possessions : new observable(new memory({})),
		abilities : new observable(new memory({})),
		money : new observable(new memory({})),
		languages : new observable(new memory({})),
		domains : new observable(new memory({})),
		knownspells : new observable(new memory({})),
		numspells : new observable(new memory({}))
	};

	var attrToBon = function(v) {
		return parseInt(Math.floor((v - 10) / 2));
	};

	var ons = [];
	var inputs = [];
	var a = "rpg:dnd3.5-obj";
	var input = (function() {

		// as this will be created on destruction, we can safely pass a value.
		return function(store, item, field) {
			var ret = construct.toDom("<div class=\"dnd3.5-input-container\"><span class=\"" + (state === states.disabled ? "" : "hidden") + "\">" + item[field] + "</span><input class=\"" + (state === states.enabled ? "" : "hidden") + "\" type=\"text\" value=\"" + item[field] + "\" /></div>");
			var ctrl = {
				disable : function() {
					dom_class.remove(ret.firstElementChild, "hidden");
					dom_class.add(ret.lastElementChild, "hidden");
				},
				enable : function() {
					dom_class.add(ret.firstElementChild, "hidden");
					dom_class.remove(ret.lastElementChild, "hidden");
				},
				store : function(v) {
					var n = store.get(item.id);
					n[field] = v;
					store.put(n);
				},
				destroy : function() {

				}
			};
			ret.lastElementChild[a] = ctrl;
			inputs.push(ctrl);
			return ret;
		};
	})();

	var createTable = function(headers) {
		var ih = "";
		if (headers) {
			ih = headers.reduce(function(prev, curr) {
				return prev + "<th>" + curr + "</th>";
			}, "<thead><tr>") + "</tr></thead>";
		}
		ih = "<table>" + ih + "<tbody></tbody></table>";
		var ret = {};
		ret.table = construct.toDom(ih);
		ret.body = ret.table.tBodies[0];
		return ret;
	};

	// insertRow {{{
	character.misc.createTable = function() { return createTable(); };
	character.misc.insertRow = function(item) {
		var ret = construct.toDom("<tr><td>" + item.id + "</td><td></td></tr>");
		construct.place(input(character.misc, item, "value"), ret.children[1], "first");
		return ret;
	};

	character.attributes.createTable = function() {
		return createTable(["Attribute", "Score", "Modifier", "Temp Score", "Temp Modifier"]);
	};
	character.attributes.insertRow = function(item) {
		var ret = construct.toDom(
			"<tr><td>" + item.id + "</td><td></td><td>" + attrToBon(item.score) + "</td><td></td><td>" + attrToBon(item.tempscore) + "</td></tr>"
		);
		
		construct.place(input(character.attributes, item, "score"), ret.children[1], "first");
		construct.place(input(character.attributes, item, "tempscore"), ret.children[3], "first");

		return ret;
	};
	character.attributes.onupdate = function(item) {
		onAttrChange(item.id, (item.tempscore ? attrToBon(item.tempscore) : attrToBon(item.score)));
	};
	character.savings.createTable = function() {
		return createTable(["Name", "Total", "", "Base", "", "Abil", "", "Magic", "", "Misc", "", "Temp Modifier"]);
	};
	character.savings.insertRow = function(item) {
		var ih = "<tr><td>" + item.id + "</td>";
		var attr = character.attributes.get(item.attr);
		var abmod = attrToBon(!!attr.tempscore ? attr.tempscore : attr.score);
		ih += "<td>" + (item.base + abmod + item.magmod + item.miscmod) + "</td>";
		ih += "<td>=</td><td></td>"; // into input
		ih += "<td>+</td><td>" + abmod + "</td>";
		ih += "<td>+</td><td></td>"; // into input
		ih += "<td>+</td><td></td>"; // into input
		ih += "<td>+</td><td></td>"; // into input
		var ret = construct.toDom(ih);
		
		construct.place(input(character.attributes, item, "base"), ret.children[3], "first");
		construct.place(input(character.attributes, item, "magmod"), ret.children[7], "first");
		construct.place(input(character.attributes, item, "miscmod"), ret.children[9], "first");
		construct.place(input(character.attributes, item, "tempmod"), ret.children[11], "first");

		return ret;
	};
	character.fight.insertRow = function(item) {

	};
	character.attack.insertRow = function(item) {

	};
	character.skills.insertRow = function(item) {

	};
	character.feats.insertRow = function(item) {

	};
	character.gear.insertRow = function(item) {

	};
	character.possessions.insertRow = function(item) {

	};
	character.abilities.insertRow = function(item) {

	};
	character.money.insertRow = function(item) {

	};
	character.languages.insertRow = function(item) {

	};
	character.domains.insertRow = function(item) {

	};
	character.knownspells.insertRow = function(item) {

	};
	character.numspells.insertRow = function(item) {

	};
	// }}}

	var onAttrChange = function(attr, new_mod) {
		character.skills.query({ attr : attr }).forEach(function(o) {
			o.abmod = new_mod;
			character.skills.put(o);
		});
		character.savings.query({ attr : attr }).forEach(function(o) {
			character.savings.put(o);
		});
	};

	// credits: http://stackoverflow.com/a/14794066
	function isInt(value) {
		if (isNaN(value)) {
			return false;
		}
		var x = parseFloat(value);
		return (x | 0) === x;
	}

	var lists = [];
	var clear = function() { // {{{
		// misc
		[
			"Character Name", "Player", "Class", "Race",
			"Alignment", "Deity", "Size", "Age", "Gender", 
			"Height", "Weight", "Eyes", "Hair", "Skin"
		].forEach(function(o) { character.misc.add({id: o, value: ""}); });

		// attributes
		[
			"Strength", "Dexterity", "Constitution", "Intelligence", "Wisdom", "Charisma"
		].forEach(function(o) {
			character.attributes.put({ id : o, score : 0, tempscore : 0 });
		});
			

		// savings
		[
			{ name : "Fortitude", attr : "Constitution" },
			{ name : "Reflex", attr : "Dexterity" },
			{ name : "Will", attr : "Wisdom" }
		].forEach(function(o) {
			character.savings.put({ id : o.name, attr : o.attr, base : 0, magmod : 0, miscmod : 0, tempmod : 0});
		});

		// fight
		[
			{ id : "Hit Points", value : 0},
			{ id : "Armor Class", armor : 0, shield : 0, size : 0, natural : 0, defl : 0, misc : 0},
			{ id : "Speed", value : 0},
			{ id : "Damage Reduction", value : 0},
			{ id : "Touch" },
			{ id : "Flat-Footed" },
			{ id : "Initiative", misc : 0},
			{ id : "Base Attack Bonus", value: 0},
			{ id : "Spell Resistance", value : 0},
			{ id : "Grapple", size: 0, misc: 0}
		].forEach(function(o) {
			character.fight.put(o);
		});

		// attack
		// empty on default

		// skills
		[
			{ id : "Appraise", attr : "Intelligence", rank : 0, misc : 0, classab : false, untrained : true},
			{ id : "Handle Animal", attr : "Charisma", rank : 0, misc : 0, classab : false, untrained : false}
		].forEach(function(o) {
			character.skills.put(o);
		});

		// feats

		// gear

		// possessions

		// abilities

		// money

		// languages

		// domains

		// knownspells

		// numspells
	}; // }}}

	var clear_store = function (store) {
		store.query().forEach(function(item) {
			store.remove(item[store.idProperty]);
		});
	};

	module["destroy"] = function() {
		construct.empty(nodes.root);
		Object.keys(character).forEach(function(key) {
			clear_store(character[key]);
		});
	};

	module["show"] = function(id) {
		id = dom.byId(id);
		if (nodes.root && nodes.root != id) module.destroy();
		nodes.root = id;
		nodes.edit = construct.toDom("<input type=\"button\" value=\"Edit\" />");
		nodes.save = construct.toDom("<input type=\"button\" class=\"hidden\" value=\"Save\" />");
		nodes.close = construct.toDom("<input type=\"button\" value=\"Close\" />");

		nodes.edit.onclick = function(e) {
			dom_class.add(nodes.edit, "hidden");
			dom_class.remove(nodes.save, "hidden");
			state = states.enabled;
			inputs.forEach(function(o) { o.enable(); });
		};
		nodes.save.onclick = function(e) {
			dom_class.remove(nodes.edit, "hidden");
			dom_class.add(nodes.save, "hidden");
			state = states.disabled;
			inputs.forEach(function(o) { o.disable(); });
		};
		nodes.close.onclick = function(e) {
			module.onclose();
		};
		nodes.sheet = construct.toDom("<div></div>");
		construct.place(nodes.edit, nodes.root, "last");
		construct.place(nodes.save, nodes.root, "last");
		construct.place(nodes.close, nodes.root, "last");
		construct.place(nodes.sheet, nodes.root, "last");

		clear();
		on(nodes.sheet, "input:change", function(e) {
			// TODO add debounce!
			console.log("[dnd3.5] input:onchange fired!");
			var v = parseInt(this.value);
			if (isInt(v)) {
				this[a].store(v);
			};
		});
		Object.keys(character).forEach(function(key) {
			var s = character[key];
			if (!s.createTable) return;
			var table = s.createTable();
			construct.place(table.table, nodes.sheet, "first");
			lists.push(list(table.body, s.query(), s.insertRow, s.ondelete, s.onupdate, s.oninsert));
		});
		
	};

	module["onclose"] = function() {};

	module["display"] = function(data) {
		data = data || {};
		if (typeof data === "object") {
			Object.keys(data).forEach(function(key) {
				var _d = data[key];
				Object.keys(_d).forEach(function(_k) {
					var old = character[key].get(_d[_k].id) || {};
					character[key].put(lang.mixin(old, _d[_k]));
				});
			});
		};
	};

	module["export"] = function() {
		return null;
	};
	return module;
});
