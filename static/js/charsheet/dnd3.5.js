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
	}
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

	var onAttrChange = function(attr, new_mod) {
		character.skills.query({ attr : attr }).forEach(function(o) {
			o.abmod = new_mod;
			character.skills.put(o);
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

	var ons = [];
	var inputs = [];
	(function() {
		var a = "rpg:dnd3.5-obj";

			// as this will be created on destruction, we can safely pass a value.
		return function(store, id, field, value) {
			var ret = construct.toDom("<div class=\"dnd3.5-input-container\"><span>" + value + "</span><input type=\"text\" value=\"" + value + "\" /></div>");
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
					var n = store.get(id);
					n[field] = v;
					store.put(n);
				},
				destroy : function() {

				}
			};
			ret.lastElementChild[a] = ctrl;
			inputs.push(ret);
			return ret;
		};
	})();

	var lists = [];
	var clear = function() { // {{{
		// misc
		[
			"Character Name", "Player", "Class", "Race",
			"Alignment", "Deity", "Size", "Age", "Gender", 
			"Height", "Weight", "Eyes", "Hair", "Skin"
		].forEach(function(o) { character.misc.add({id: o}); });
		character.misc.display = function(container) {
			lists.push(list(container, character.misc.query(), function insertRow(item) {
				var ret = 
			});

		};

		// attributes
		[
			"Strength", "Dexterity", "Constitution", "Intelligence", "Wisdom", "Charisma"
		].forEach(function(o) {
			character.attributes.put({ id : o, score : 0, tempscore : null });
		});
			

		// savings
		[
			{ name : "Fortitude", attr : "Constitution" },
			{ name : "Reflex", attr : "Dexterity" },
			{ name : "Will", attr : "Wisdom" }
		].forEach(function(o) {
			character.savings.put({ id : o.name, attr : o.attr, base : 0, magmod : 0, miscmod : 0 });
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

		nodes.edit.onclick = function(e) {
			dom_class.add(nodes.edit, "hidden");
			dom_class.remove(nodes.save, "hidden");
			inputs.forEach(function(o) { o.enable(); });
		};
		nodes.save.onclick = function(e) {
			dom_class.remove(nodes.edit, "hidden");
			dom_class.edit(nodes.save, "hidden");
			inputs.forEach(function(o) { o.disable(); });
		};
		nodes.sheet = construct.toDom("<div></div>");
		construct.place(nodes.edit, nodes.root, "last");
		construct.place(nodes.save, nodes.root, "last");
		construct.place(nodes.sheet, nodes.root, "last");

		clear();
		on(nodes.sheet, "input:onchange", function(e) {
			// TODO add debounce!
			console.log("[dnd3.5] input:onchange fired!");
			var v = parseInt(this.value);
			if (isInt(v)) {
				this[a].store(v);
			};
		});
		Object.keys(character).forEach(function(key) {

		});
		
	};

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
