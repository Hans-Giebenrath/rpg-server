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
define(["dojo/request",
		 "rpg/log"
], function (request, log) {
	//var self = {
		//send : function (method, data, success, error) {
	var send = function (method, data, success, error) {
		var q = {};
		var i = 1;
		data.forEach(function(item) {
			q["arg" + i] = item;
			i = i + 1;
		});
		request("/ajax/" + method, {
			data : q,
			handleAs : "json",
			preventCache : true,
			method : "POST"
		}).then(function (data) {
			if (typeof success === "function") {
				if (data == "null") {
					data = null;
				}
			   success(data);
			}
		}, function (err) {
			log.error(err.response.message, err);
			if (typeof error === "function") error();
		});
	}
	return send;
});
