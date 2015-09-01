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
define(["rpg/store"], function(store) {
	return {
		_prefix : "static/image/",
		_splitting : function (url) {
			return this._prefix + url.substr(0,2) + "/" + url.substr(2,2) + "/";
		},
		_base : function (url) {
			return this._splitting(url) + url;
		},
		small : function (url) {
			return this._base(url) + ".small";
		},
		medium : function (url) {
			return this._base(url) + ".medium";
		},
		orig : function (url, id) {
			var orig = store.image.get(id).orig;

			return this._splitting(url) + orig + ":" + url + ".orig";
		}
	};
});

