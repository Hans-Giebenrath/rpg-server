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
		"rpg/rndm",
		"rpg/soul"
], function (dom, lang, construct, dom_class, attr, observable, memory, debounce, on, list, ajax, overlay, rndm, soul) {
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
	var getAttrBonus = function(v) {
		var o = v;
		if (typeof v === "string") {
			o = character.attributes.get(v);
		}
		if (o.tempscore === "") {
			return attrToBon(o.score);
		} else {
			return attrToBon(o.tempscore);
		}
	};

	var ons = [];
	var inputs = [];
	var a = "rpg:dnd3.5-obj";
	var input = (function() {

		// as this will be created on destruction, we can safely pass a value.
		return function(store, item, field, caption) {
			var ret = construct.toDom("<span class=\"dnd3.5-input-container\"><span class=\"" + (state === states.disabled ? "" : "hidden") + "\">" + item[field] + "</span><input class=\"" + (state === states.enabled ? "" : "hidden") + "\" type=\"text\" value=\"" + item[field] + "\" /></span>");
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
			if (caption && typeof caption === "string") {
				attr.set(ret.firstElementChild, "data-caption", caption);
			}
			ret.lastElementChild[a] = ctrl;
			inputs.push(ctrl);
			return ret;
		};
	})();

	// if onexpand is function, then it will be the click
	// handler of that function
	var createTable = function(headers, onexpand) {
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
	var createSection = function() {
		var tmp = construct.toDom("<div></div>");
		return { table : tmp, body : tmp };
	};

	// insertRow {{{
	character.insertData = function (data) {
		var self = this;
		if (Array.isArray(data)) {
			data.forEach(function(o) {
				self.insertData(o);
			});
			return;
		}

		if (typeof data === "string") {
			data = { id : data };
		}
		self.put(lang.mixin(lang.clone(self.std), data));
	}
	Object.keys(character).forEach(function(k) {
		character[k].insertData = character.insertData;
	});
	character.misc.std = { id : "", value : "" };
	character.misc.createTable = function() { return createSection(); };
	character.misc.insertRow = function(item) {
		var ret = construct.toDom("<tr><td>" + item.id + "</td><td></td></tr>");
		construct.place(input(character.misc, item, "value"), ret.children[1], "first");
		return ret;
	};

	character.attributes.std = { id : "", score : 0, tempscore : 0 };
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
		onAttrChange(item.id, getAttrBonus(item));
	};

	character.savings.std = { id : "", attr : "", base : 0, magmod : 0, miscmod : 0, tempmod : 0 };
	character.savings.createTable = function() {
		return createTable(["Name", "Total", "", "Base", "", "Abil", "", "Magic", "", "Misc", "", "Temp Modifier"]);
	};
	character.savings.insertRow = function(item) {
		var ih = "<tr><td>" + item.id + "</td>";
		var abmod = getAttrBonus(item.attr);
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
	character.fight.std = { id : "",  };
	character.fight.createTable = function() { return createSection(); };
	character.fight.insertRow = function(item) {
		var ret;
		switch(item.id) {
			case "Hit Points":
			case "Speed":
			case "Damage Reduction":
			case "Touch":
			case "Flat-Footed":
			case "Base Attack Bonus":
			case "Spell Resistance":
			case "Spell Save":
			case "Arcane Spell Failure":
				var ih = "<div><span>" + item.id + "</span><span></span></div>";
				ret = construct.toDom(ih);
				construct.place(input(character.fight, item, "value"), ret.children[1], "first");
				break;
			case "Armor Class":
				var total = 10 + parseInt(item.armor) + parseInt(item.shield) + parseInt(item.size) + parseInt(item.natural) + parseInt(item.defl) + parseInt(item.misc) + getAttrBonus("Dexterity");
				var ih = "<div><span>Hit Points</span><span>" + total + "</span><span>=10</span>";
				ih += "<span>+</span><span></span>";
				ih += "<span>+</span><span></span>";
				ih += "<span data-caption=\"Dex Modifier\">+" + getAttrBonus("Dexterity") + "</span>";
				ih += "<span>+</span><span></span>";
				ih += "<span>+</span><span></span>";
				ih += "<span>+</span><span></span>";
				ih += "<span>+</span><span></span>";
				ret = construct.toDom(ih);
				construct.place(input(character.fight, item, "armor", "Armor Bonus"), ret.children[4], "first");
				construct.place(input(character.fight, item, "shield", "Shield Bonus"), ret.children[6], "first");
				construct.place(input(character.fight, item, "size", "Size Modifier"), ret.children[9], "first");
				construct.place(input(character.fight, item, "natural", "Natural Armor"), ret.children[11], "first");
				construct.place(input(character.fight, item, "defl", "Deflection Modifier"), ret.children[13], "first");
				construct.place(input(character.fight, item, "misc", "Misc Modifier"), ret.children[15], "first");
				break;
			case "Initiative":
				var total = getAttrBonus("Dexterity") + parseInt(item.misc);
				var ih = "<div><span>Initiative</span><span>" + total + "</span><span>=</span>";
				ih += "<span data-caption=\"Dex Modifier\">" + getAttrBonus("Dexterity") + "</span>";
				ih += "<span>+</span><span></span>";
				ret = construct.toDom(ih);
				construct.place(input(character.fight, item, "misc", "Misc Modifier"), ret.children[5], "first");
				break;
			case "Grapple":
				break;
			default: console.error("[character.fight.insertRow] Not supported id.", item);
		};
		return ret;
	};
	character.fight.onupdate = function(item) {
		if (item.id == "Base Attack Bonus") {
			var o = character.fight.get("Grapple");
			character.fight.put(o);
		}
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
		if (attr == "Dexterity") {
			character.misc.put(character.misc.get("Armor Class"));
			character.misc.put(character.misc.get("Initiative"));
		}
		if (attr == "Strength") {
			character.fight.put(character.fight.get("Grapple"));
		}
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
		character.misc.insertData([
			"Character Name", "Player", "Class", "Race",
			"Alignment", "Deity", "Size", "Age", "Gender", 
			"Height", "Weight", "Eyes", "Hair", "Skin",
			"Campaign", "Experience Points"
		]);

		// attributes
		character.attributes.insertData([
			"Strength", "Dexterity", "Constitution", "Intelligence", "Wisdom", "Charisma"
		]);
			
		// savings
		character.savings.insertData([
			{ id : "Fortitude", attr : "Constitution" },
			{ id : "Reflex", attr : "Dexterity" },
			{ id : "Will", attr : "Wisdom" }
		]);

		// fight
		character.fight.insertData([
			{ id : "Hit Points", value : 0},
			{ id : "Armor Class", armor : 0, shield : 0, size : 0, natural : 0, defl : 0, misc : 0},
			{ id : "Speed", value : 0},
			{ id : "Damage Reduction", value : 0},
			{ id : "Touch", value : 0 },
			{ id : "Flat-Footed", value : 0 },
			{ id : "Initiative", misc : 0},
			{ id : "Base Attack Bonus", value: 0},
			{ id : "Spell Resistance", value : 0},
			{ id : "Grapple", size: 0, misc: 0},
			{ id : "Spell Save", value : 0},
			{ id : "Arcane Spell Failure", value : 0},
		]);

		// attack
		// empty on default

		// skills
		character.skills.insertData([
			{ id : "Appraise", attr : "Intelligence", untrained : true},
			{ id : "Balance", attr : "Dexterity", untrained : true},
			{ id : "Bluff", attr : "Charisma", untrained : true},
			{ id : "Climb", attr : "Strength", untrained : true},
			{ id : "Concentration", attr : "Constitution", untrained : true},
			{ id : "Craft (...)", attr : "Intelligence", untrained : true},
			{ id : "Decipher Script", attr : "Intelligence", untrained : false},
			{ id : "Diplomacy", attr : "Charisma", untrained : true},
			{ id : "Disable Device", attr : "Intelligence", untrained : false},
			{ id : "Disguise", attr : "Charisma", untrained : true},
			{ id : "Escape Artist", attr : "Dexterity", untrained : true},
			{ id : "Gorgery", attr : "Intelligence", untrained : true},
			{ id : "Gather Information", attr : "Charisma", untrained : true},
			{ id : "Handle Animal", attr : "Charisma", untrained : false},
			{ id : "Heal", attr : "Wisdom", untrained : true},
			{ id : "Hide", attr : "Dexterity", untrained : true},
			{ id : "Intimidate", attr : "Charisma", untrained : true},
			{ id : "Jump", attr : "Strength", untrained : true},
			{ id : "Knowledge (...)", attr : "Intelligence", untrained : false},
			{ id : "Listen", attr : "Wisdom", untrained : true},
			{ id : "Move Silently", attr : "Dexterity", untrained : true},
			{ id : "Open Lock", attr : "Dexterity", untrained : false},
			{ id : "Perform (...)", attr : "Charisma", untrained : false},
			{ id : "Profession (...)", attr : "Wisdom", untrained : false},
			{ id : "Ride", attr : "Dexterity", untrained : true},
			{ id : "Search", attr : "Intelligence", untrained : true},
			{ id : "Sense Motive", attr : "Wisdom", untrained : true},
			{ id : "Sleight of Hand", attr : "Dexterity", untrained : false},
			{ id : "Spellcraft", attr : "Intelligence", untrained : false},
			{ id : "Spot", attr : "Wisdom", untrained : true},
			{ id : "Survival", attr : "Wisdom", untrained : true},
			{ id : "Swim", attr : "Strength", untrained : true},
			{ id : "Tumble", attr : "Dexterity", untrained : false},
			{ id : "Use Magic Device", attr : "Charisma", untrained : false},
			{ id : "Use Rope", attr : "Dexterity", untrained : true},
		]);

		// feats

		// gear

		// possessions

		// abilities

		// money
		character.money.insertData([ "CP", "SP", "GP", "PP" ]);

		// languages

		// domains

		// knownspells

		// numspells
		character.numspells.insertData(["0", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th"]);
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
