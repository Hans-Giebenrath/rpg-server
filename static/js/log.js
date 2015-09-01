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
	    "rpg/modal"
], function(dom, modal) {
	return {
		error : function(msg, o) {
			msg = msg || o.response.data;
			console.error(msg, o);
			var m = modal("An Error occured.", { "desc" : msg}, [], [ { "Ok, I got it." : function(inputs, close) { close(); }}, {"No, I didn't got it." : function() { alert("Then read again."); }}], null, function () { m.destroy(); });
			m.show();
			// TODO do some user-stuff
		},
		info : function(msg) {
			var m = modal("An information for you came in.", { "desc" : msg}, [], [ { "Ok, I got it." : function(inputs, close) { close(); }}, {"No, I didn't got it." : function() { alert("Then read again."); }}], null, function () { m.destroy(); });
			// TODO do some user-stuff
		},
		success : function(msg) {
			// TODO do some user-stuff
		}
	};
});
