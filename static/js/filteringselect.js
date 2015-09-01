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

define(["dojo/dom",
	    "dojo/dom-construct",
		"rpg/list"
], function(dom, construct, list) {
	var create_option = function(attr_value, attr_display, item) {
		return construct.toDom("<option value=\"" + item[attr_value] + "\">" + item[attr_display] + "</option>");
	};

	return function (id, observable, attr_value, attr_display) {
		var select = construct.place("<select></select>", id, "first");

		var destr = list(select, observable, function(item) { return create_option(attr_value, attr_display, item); });

		return {
			value : function() {
				return select.value;
			},
			destroy : function () {
				destr();
				construct.empty(id);
			}
		};
	};
});
